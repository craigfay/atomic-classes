
use serde::{Deserialize, Serialize};

use std::collections::BTreeMap;
use std::fs::File;
use std::io::BufReader;
use std::fs;

#[derive(Deserialize, Debug)]
#[serde(rename_all = "camelCase")]
#[serde(tag = "method")]
pub enum Transformation {
    SingleRuleFromInputGroup(FromInputGroup),
    ManyRulesFromInputGroup(ManyRulesFromInputGroup),
    CopyExistingRules(CopyExistingRules),
}

#[derive(Deserialize, Debug)]
#[serde(rename_all = "camelCase")]
pub struct CopyExistingRules{
    id: String,
    description: String,
    affected_ids: Vec<String>,
    #[serde(rename = "@identifier")]
    at_rule_identifier: Option<String>,
    new_selector: String,
}


#[derive(Deserialize, Debug)]
#[serde(rename_all = "camelCase")]
pub struct FromInputGroup {
    id: String,
    description: String,
    input_group_name: String,
    selector: String,
    declarations: BTreeMap<String, String>
}

#[derive(Deserialize, Debug)]
#[serde(rename_all = "camelCase")]
pub struct ManyRulesFromInputGroup {
    id: String,
    description: String,
    input_group_name: String,
    rules: Vec<CSSRule>,
}


pub type InputGroup = BTreeMap<String, String>;

#[derive(Deserialize, Debug)]
#[serde(rename_all = "camelCase")]
struct Config {
    input_groups: BTreeMap<String, InputGroup>,
    transformations: Vec<Transformation>,
}

#[derive(Deserialize, Serialize, Debug, Default, Clone)]
struct CSSRule {
    selector: String,
    declarations: BTreeMap<String, String>,
}

fn err_msg_for_missing_map(input_group_name: &str) -> String {
    format!(
        "There is no input group named \"{}\"",
        input_group_name,
    )
}

fn err_msg_for_missing_transformation(description: &str, id: &str) -> String {
    format!(
        "{}: There is no transformation named {}",
        description,
        id
    )
}

/// Derive a single `CSSRule` using `FromInputGroup`
fn many_rules_from_input_group_name(
    config: &Config,
    transformation: &ManyRulesFromInputGroup,
    intermediate: &mut Intermediate,
) {
    let input_group_name = config.input_groups
        .get(&transformation.input_group_name)
        .expect(&err_msg_for_missing_map(&transformation.input_group_name));

    let mut rules = vec![];

    for rule in &transformation.rules {
        for (var_key, var_val) in input_group_name {
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

    intermediate.normal_rules.insert(transformation.id.clone(), RuleFamily {
        description: transformation.description.clone(),
        css_rules: rules,
    });
}

/// Derive a single `CSSRule` using `FromInputGroup`
fn single_rule_from_input_group_name(
    config: &Config,
    transformation: &FromInputGroup,
    intermediate: &mut Intermediate
) {
    let input_group = config.input_groups
        .get(&transformation.input_group_name)
        .expect(&err_msg_for_missing_map(&transformation.input_group_name));

    let selector = transformation.selector.clone();
    let mut declarations = BTreeMap::new();

    for (var_key, var_val) in input_group {
        let inject_variables = |s: &String| s
            .replace("{{ KEY }}", var_key)
            .replace("{{ VAL }}", var_val);

        for (property, value) in &transformation.declarations {
            declarations.insert(
                inject_variables(&property),
                inject_variables(&value),
            );
        }
    }

    intermediate.normal_rules.insert(transformation.id.clone(), RuleFamily {
        description: transformation.description.clone(),
        css_rules: vec![CSSRule { selector, declarations }]
    });
}

/// Copy existing rules into a media query block
fn copy_existing_rules(
    transformation: &CopyExistingRules,
    intermediate: &mut Intermediate,
) {
    let mut new_rules: Vec<CSSRule> = vec![];

    for id in &transformation.affected_ids {
        let rule_family = intermediate.normal_rules.get(&id.clone())
            .expect(&err_msg_for_missing_transformation(&transformation.description, &id));

        for rule in rule_family.css_rules.iter() {
            let mut selector = transformation.new_selector.clone();

            let prev_class_name = rule.selector.replacen(".", "", 1);
            selector = selector.replace("{{ PREV_SELECTOR_CLASS_NAME }}", &prev_class_name);
            selector = selector.replace("{{ PREV_SELECTOR }}", &rule.selector);

            new_rules.push(CSSRule {
                selector,
                ..rule.clone()
            });
        }
    }

    match &transformation.at_rule_identifier {
        Some(identifier) => {
            intermediate.at_rules.insert(transformation.id.clone(), AtRule {
                identifier: identifier.clone(),
                description: transformation.description.clone(),
                css_rules: new_rules,
            });
        },
        None => {
            intermediate.normal_rules.insert(transformation.id.clone(), RuleFamily {
                description: transformation.description.clone(),
                css_rules: new_rules,
            });
        }
    }


}

type TransformationID = String;

#[derive(Default, Serialize)]
struct Intermediate {
    normal_rules: BTreeMap<TransformationID, RuleFamily>,
    at_rules: BTreeMap<TransformationID, AtRule>,
}

#[derive(Default, Serialize)]
struct RuleFamily {
    description: String,
    css_rules: Vec<CSSRule>,
}

#[derive(Default, Serialize)]
struct AtRule {
    identifier: String,
    description: String,
    css_rules: Vec<CSSRule>,
}

fn generate_rules(config: Config) -> Intermediate {
    let mut intermediate = Intermediate::default();

    for transformation in &config.transformations {
        match transformation {
            Transformation::SingleRuleFromInputGroup(transformation) => {
                single_rule_from_input_group_name(&config, &transformation, &mut intermediate);
            }
            Transformation::ManyRulesFromInputGroup(transformation) => {
                many_rules_from_input_group_name(&config, &transformation, &mut intermediate);
            }
            Transformation::CopyExistingRules(transformation) => {
                copy_existing_rules(transformation, &mut intermediate);
            }
        }
    }

    intermediate
}

fn stringify_intermediate(intermediate: &Intermediate) -> String {
    let mut css = String::new();

    for (_id, rule_family) in &intermediate.normal_rules {
        let block = stringify_rules(&rule_family.css_rules);
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
    let intermediate = serde_json::to_string_pretty(&intermediate).unwrap();
    fs::write("./build.css", css).expect("Unable to write file");
    fs::write("./intermediate.json", intermediate).expect("Unable to write file");
}
