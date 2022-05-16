
use serde::{Deserialize, Serialize};

use std::collections::BTreeMap;
use std::fs::File;
use std::io::BufReader;
use std::fs;

#[derive(Deserialize, Debug)]
#[serde(rename_all = "camelCase")]
#[serde(tag = "method")]
pub enum Instruction {
    SingleRuleFromVariableGroup(FromVariableGroup),
    ManyRulesFromVariableGroup(ManyRulesFromVariableGroup),
    AddClassModifier(AddClassModifier),
    CopyExistingRulesIntoAtRule(CopyExistingRulesIntoAtRule),
}

#[derive(Deserialize, Debug)]
#[serde(rename_all = "camelCase")]
pub struct CopyExistingRulesIntoAtRule {
    id: String,
    description: String,
    affected_ids: Vec<String>,
    identifier: String,
    selectors_become: String,
}

#[derive(Deserialize, Debug)]
#[serde(rename_all = "camelCase")]
pub struct AddClassModifier {
    id: String,
    description: String,
    affected_ids: Vec<String>,
    class_prefix: String,
    represents_pseudo_class: String,
}

#[derive(Deserialize, Debug)]
#[serde(rename_all = "camelCase")]
pub struct FromVariableGroup {
    id: String,
    description: String,
    variable_group: String,
    selector: String,
    declarations: BTreeMap<String, String>
}

#[derive(Deserialize, Debug)]
#[serde(rename_all = "camelCase")]
pub struct ManyRulesFromVariableGroup {
    id: String,
    description: String,
    variable_group: String,
    rules: Vec<CSSRule>,
}


pub type VariableGroup = BTreeMap<String, String>;

#[derive(Deserialize, Debug)]
#[serde(rename_all = "camelCase")]
struct Config {
    variable_groups: BTreeMap<String, VariableGroup>,
    instructions: Vec<Instruction>,
}

#[derive(Deserialize, Serialize, Debug, Default, Clone)]
struct CSSRule {
    selector: String,
    declarations: BTreeMap<String, String>,
}

fn err_msg_for_missing_map(variable_group_name: &str) -> String {
    format!(
        "There is no variable map named \"{}\"",
        variable_group_name,
    )
}

fn err_msg_for_missing_instruction(description: &str, id: &str) -> String {
    format!(
        "{}: There is no instruction named {}",
        description,
        id
    )
}

/// Derive a single `CSSRule` using `FromVariableGroup`
fn many_rules_from_variable_group(config: &Config, inst: &ManyRulesFromVariableGroup) -> Vec<CSSRule> {
    let variable_group = config.variable_groups
        .get(&inst.variable_group)
        .expect(&err_msg_for_missing_map(&inst.variable_group));

    let mut rules = vec![];

    for rule in &inst.rules {
        for (var_key, var_val) in variable_group {
            let inject_variables = |s: &String| s
                .replace("{{ KEY }}", var_key)
                .replace("{{ VAL }}", var_val);
            
            rules.push(CSSRule {
                selector: inject_variables(&rule.selector),
                declarations: rule.declarations.iter().map(|(property, value)| {
                    (
                        inject_variables(&property),
                        inject_variables(&value),
                    )
                }).collect()
            })
        }

    }

    rules
}

/// Derive a single `CSSRule` using `FromVariableGroup`
fn single_rule_from_variable_group(config: &Config, inst: &FromVariableGroup) -> Vec<CSSRule> {
    let variable_group = config.variable_groups
        .get(&inst.variable_group)
        .expect(&err_msg_for_missing_map(&inst.variable_group));

    let selector = inst.selector.clone();
    let mut declarations = BTreeMap::new();

    for (var_key, var_val) in variable_group {
        let inject_variables = |s: &String| s
            .replace("{{ KEY }}", var_key)
            .replace("{{ VAL }}", var_val);

        for (property, value) in &inst.declarations {
            declarations.insert(
                inject_variables(&property),
                inject_variables(&value),
            );
        }
    }

    vec![
        CSSRule { selector, declarations }
    ]
}

/// Derive a new `CSSRule` for each existing one that was created
/// by an instruction with an ID in `affected_ids`
fn add_class_modifier(
    inst: &AddClassModifier,
    intermediate: &Intermediate,
) -> Vec<CSSRule> {
    let mut new_rules: Vec<CSSRule> = vec![];

    for id in &inst.affected_ids {
        let rules = intermediate.normal_rules.get(&id.clone())
            .expect(&err_msg_for_missing_instruction(&inst.description, &id));

        for rule in rules.iter() {

            // Skipping rules that don't target classes
            if !rule.selector.starts_with(".") { continue }

            let with_prefix = rule.selector.replacen(".", &inst.class_prefix, 1);
            let selector = format!(".{}:{}", with_prefix, inst.represents_pseudo_class);

            new_rules.push(CSSRule {
                selector,
                ..rule.clone()
            });
        }
    }

    new_rules
}

/// Copy existing rules into a media query block
fn copy_existing_rules_into_media_rule(
    inst: &CopyExistingRulesIntoAtRule,
    intermediate: &mut Intermediate,
) {
    let mut new_rules: Vec<CSSRule> = vec![];

    for id in &inst.affected_ids {
        let rules = intermediate.normal_rules.get(&id.clone())
            .expect(&err_msg_for_missing_instruction(&inst.description, &id));

        for rule in rules.iter() {
            let mut selector = inst.selectors_become.clone();

            let prev_class_name = rule.selector.replacen(".", "", 1);
            selector = selector.replace("{{ PREV_CLASS_NAME }}", &prev_class_name);
            selector = selector.replace("{{ PREV_SELECTOR }}", &rule.selector);

            new_rules.push(CSSRule {
                selector,
                ..rule.clone()
            });
        }
    }

    intermediate.at_rules.insert(inst.id.clone(), AtRule {
        identifier: inst.identifier.clone(),
        css_rules: new_rules,
    });
}

type InstructionID = String;

#[derive(Default, Serialize)]
struct Intermediate {
    normal_rules: BTreeMap<InstructionID, Vec<CSSRule>>,
    at_rules: BTreeMap<InstructionID, AtRule>,
}

#[derive(Default, Serialize)]
struct AtRule {
    identifier: String,
    css_rules: Vec<CSSRule>,
}

fn generate_rules(config: Config) -> Intermediate {
    let mut intermediate = Intermediate::default();

    for instruction in &config.instructions {
        match instruction {
            Instruction::SingleRuleFromVariableGroup(inst) => {
                intermediate.normal_rules.insert(inst.id.clone(), single_rule_from_variable_group(&config, &inst));
            }
            Instruction::ManyRulesFromVariableGroup(inst) => {
                intermediate.normal_rules.insert(inst.id.clone(), many_rules_from_variable_group(&config, &inst));
            }
            Instruction::AddClassModifier(inst) => {
                intermediate.normal_rules.insert(inst.id.clone(), add_class_modifier(&inst, &intermediate));
            },
            Instruction::CopyExistingRulesIntoAtRule(inst) => {
                copy_existing_rules_into_media_rule(inst, &mut intermediate);
            }
        }
    }

    intermediate
}

fn stringify_intermediate(intermediate: &Intermediate) -> String {
    let mut css = String::new();

    for (_id, css_rules) in &intermediate.normal_rules {
        let block = stringify_rules(css_rules);
        css = format!("{}{}", css, block);
    }

    for (_id, at_rule) in &intermediate.at_rules {
        let mut block = stringify_rules(&at_rule.css_rules);
        block = format!("{} {{\n{}}}", at_rule.identifier, block);
        css = format!("{}{}", css, block);
    }

    css   
}

fn stringify_rules(rules: &Vec<CSSRule>) -> String {
    let mut css = String::new();

    for rule in rules {
        let inner = rule.declarations.iter()
            .map(|(k, v)| format!("{}:{};", k, v))
            .collect::<Vec<String>>()
            .join("");

        let line = format!("{}{{{}}}\n", rule.selector, inner);
        css.push_str(&line);
    }

    css
}

fn main() {
    let path = std::env::args().nth(1)
        .unwrap_or("config.json".to_string());

    let file = File::open(path).unwrap();
    let reader = BufReader::new(file);
    let config: Config = serde_json::from_reader(reader).unwrap();
    let intermediate = generate_rules(config);
    let css = stringify_intermediate(&intermediate);
    let intermediate = serde_json::to_string(&intermediate).unwrap();
    fs::write("./build.css", css).expect("Unable to write file");
    fs::write("./intermediate.json", intermediate).expect("Unable to write file");
}
