// GOSH I FUCKING LOVE SERDE
use serde::{Deserialize, Serialize};
use std::collections::HashMap;

#[derive(Serialize, Deserialize, Debug)]
pub struct CodeTemplate {
    pub blocks: Vec<CodeBlock>,
}

#[derive(Serialize, Deserialize, Debug)]
pub struct CodeBlock {
    pub id: String,
    #[serde(flatten)]
    pub info: Info,
}

#[derive(Serialize, Deserialize, Debug)]
#[serde(untagged)]
pub enum Info {
    BlockInfo(BlockInfo),
    BracketInfo(BracketInfo),
}

#[derive(Serialize, Deserialize, Debug)]
#[serde(tag = "block")]
pub enum BlockInfo {
    #[serde(rename = "event")]
    PlayerEvent { args: Args, action: String },
    #[serde(rename = "entity_event")]
    EntityEvent { args: Args, action: String },
    #[serde(rename = "func")]
    Function { args: Args, data: String },
    #[serde(rename = "process")]
    Process { args: Args, data: String },
    #[serde(rename = "call_func")]
    CallFunction { args: Args, data: String },
    #[serde(rename = "start_process")]
    StartProcess { args: Args, data: String },
    #[serde(rename = "if_var")]
    IfVariable { args: Args, action: String },
    #[serde(rename = "if_player")]
    IfPlayer { args: Args, action: String },
    #[serde(rename = "if_game")]
    IfGame { args: Args, action: String },
    #[serde(rename = "if_entity")]
    IfEntity { args: Args, action: String },
    #[serde(rename = "else")]
    Else,
    #[serde(rename = "set_var")]
    SetVariable { args: Args, action: String },
    #[serde(rename = "player_action")]
    PlayerAction { args: Args, action: String },
    #[serde(rename = "game_action")]
    GameAction { args: Args, action: String },
    #[serde(rename = "entity_action")]
    EntityAction { args: Args, action: String },
    #[serde(rename = "control")]
    Control { args: Args, action: String },
    #[serde(rename = "repeat")]
    Repeat { args: Args, action: String },
    // TODO: Select, NOT
}

#[derive(Serialize, Deserialize, Debug)]
pub struct BracketInfo {
    #[serde(rename = "direct")]
    pub direction: BracketDirection,
    #[serde(rename = "type")]
    pub typ: BracketType,
}

#[derive(Serialize, Deserialize, Debug, Default)]
pub struct Args {
    pub items: Vec<CodeItem>,
}

#[derive(Serialize, Deserialize, Debug)]
pub struct CodeItem {
    pub item: CodeValue,
    pub slot: i32,
}

#[derive(Serialize, Deserialize, Debug)]
#[serde(tag = "id", content = "data")]
pub enum CodeValue {
    #[serde(rename = "txt")]
    Text { name: String },
    #[serde(rename = "num")]
    Number { name: String },
    #[serde(rename = "var")]
    Variable { name: String, scope: VariableScope },
    #[serde(rename = "loc")]
    Location {
        #[serde(rename = "is_block")]
        is_block: bool,
        x: f64,
        y: f64,
        z: f64,
        pitch: f64,
        yaw: f64,
    },
    #[serde(rename = "vec")]
    Vector { x: f64, y: f64, z: f64 },
    #[serde(rename = "snd")]
    Sound {
        sound: String,
        pitch: f64,
        #[serde(rename = "vol")]
        volume: f64,
        variant: Option<String>,
    },
    #[serde(rename = "pot")]
    Potion {
        #[serde(rename = "pot")]
        potion: String,
        #[serde(rename = "dur")]
        duration: f64,
        #[serde(rename = "amp")]
        amplitude: f64,
    },
    #[serde(rename = "g_val")]
    GameValue {
        #[serde(rename = "type")]
        typ: String,
        target: String,
    },
    #[serde(rename = "bl_tag")]
    Tag {
        option: String,
        tag: String,
        action: String,
        block: String,
    },
    #[serde(rename = "part")]
    Particle {
        particle: String,
        cluster: ParticleCluster,
        data: HashMap<String, serde_json::Value>,
    },
    #[serde(rename = "item")]
    Item {
        #[serde(rename = "item")]
        nbt: String,
    },
}
// TODO: particles, spawn eggs?

#[derive(Serialize, Deserialize, Debug)]
pub enum VariableScope {
    #[serde(rename = "saved")]
    Save,
    #[serde(rename = "unsaved")]
    Global,
    #[serde(rename = "local")]
    Local,
}

#[derive(Serialize, Deserialize, Debug)]
pub enum BracketDirection {
    #[serde(rename = "open")]
    Open,
    #[serde(rename = "close")]
    Close,
}

#[derive(Serialize, Deserialize, Debug)]
pub enum BracketType {
    #[serde(rename = "norm")]
    Normal,
    #[serde(rename = "repeat")]
    Repeat,
}

#[derive(Serialize, Deserialize, Debug)]
pub struct ParticleCluster {
    pub amount: i32,
    pub horizontal: f64,
    pub vertical: f64,
}
