use std::sync::{Mutex, Arc, Weak};
use crate::connections::{RunningPassiveConnection, PassiveConnection};
use crate::connections::signal_1::{FwdPreS1, FwdPostS1};
use crate::connections::signal_1::{PreAgentComponentS1, PostAgentComponentS1};
use crate::connections::signal_2::{FwdPreS2, FwdPostS2};
use crate::connections::signal_2::{PreAgentComponentS2, PostAgentComponentS2};
use crate::agents::{Agent, AgentEvent, Generator, Acceptor};
use crate::supervisor::{RunMode};

pub struct Model {
    pre_module_s1: PreAgentComponentS1,
    post_module_s1: PostAgentComponentS1,
    pre_module_s2: PreAgentComponentS2,
    post_module_s2: PostAgentComponentS2,
    gen_value: i32,
    proc_value: i32,
    event_cond: Option<i32>,
    stock: Vec<FwdEndProduct>,
}

struct FwdEndProduct {
    pub msg_gen: i32,
    pub msg_prop: i32,
    pub msg_proc: i32,
}

impl Generator<FwdPreS1, FwdPostS1> for Model {
    fn add_out_passive<C> (&mut self, connection: Weak<Mutex<C>>)
    where C: 'static + PassiveConnection<FwdPreS1, FwdPostS1> + Send
    {
        self.pre_module_s1.add_connection(connection);
    }
}

impl Acceptor<FwdPreS1, FwdPostS1> for Model {
    fn add_in<C> (&mut self, connection: Weak<Mutex<C>>)
    where C: 'static + PassiveConnection<FwdPreS1, FwdPostS1> + Send
    {
        self.post_module_s1.add_connection(connection);
    }
}

impl Generator<FwdPreS2, FwdPostS2> for Model {
    fn add_out_passive<C> (&mut self, connection: Weak<Mutex<C>>)
    where C: 'static + PassiveConnection<FwdPreS2, FwdPostS2> + Send
    {
        self.pre_module_s2.add_connection(connection);
    }
}

impl Acceptor<FwdPreS2, FwdPostS2> for Model {
    fn add_in<C> (&mut self, connection: Weak<Mutex<C>>)
    where C: 'static + PassiveConnection<FwdPreS2, FwdPostS2> + Send
    {
        self.post_module_s2.add_connection(connection);
    }
}

impl Agent for Model {
    fn config_run(&mut self, mode: RunMode) {
        match (mode, self.mode()) {
            (RunMode::Idle, _) => println!("config_run for mode Idle, no effect."),
            (_, RunMode::Idle) => {
                self.pre_module_s1.config_run(mode);
                self.post_module_s1.config_run(mode);
                self.pre_module_s2.config_run(mode);
                self.post_module_s2.config_run(mode);
            },
            (_, _) => panic!("call config_run when agent not idle!")
        }
    }

    fn config_idle(&mut self) {
        match &self.mode() {
            RunMode::Idle => println!("config_idel at mode Idle, no effect."),
            RunMode::Feedforward => {
                self.pre_module_s1.config_idle();
                self.post_module_s1.config_idle();
                self.pre_module_s2.config_idle();
                self.post_module_s2.config_idle();
            },
        }
    }

    fn running_connections(&self) -> Vec<RunningPassiveConnection> {
        let mut v1 = self.pre_module_s1.running_connections();
        let mut v2 = self.pre_module_s2.running_connections();
        v1.append(&mut v2);
        v1
    }
    
    fn end(&mut self) {
        self.accept();
    }
    
    fn evolve(&mut self) -> AgentEvent {
        self.accept();
        self.proc_value += 1;
        self.gen_value += 1;
        match self.event_cond {
            None => {
                // println!("agnet a go on. gen: {}, proc: {}.",  self.gen_value, self.proc_value);
                AgentEvent::N   
            },
            Some(n) => {
                match self.proc_value % n {
                    0 => {
                        println!("agnet a fire. gen: {}, proc: {}.",  self.gen_value, self.proc_value);
                        self.generate();
                        AgentEvent::Y
                    },
                    _ => {
                        // println!("agnet a go on. gen: {}, proc: {}.",  self.gen_value, self.proc_value);
                        AgentEvent::N
                    },
                }
            }
        }
    }
}

impl Model {
    pub fn new(gen_value: i32, proc_value: i32, event_cond: Option<i32>) -> Arc<Mutex<Model>> {
        Arc::new(Mutex::new(
            Model{
                pre_module_s1: PreAgentComponentS1::new(),
                post_module_s1: PostAgentComponentS1::new(),
                pre_module_s2: PreAgentComponentS2::new(),
                post_module_s2: PostAgentComponentS2::new(),
                gen_value,
                proc_value,
                event_cond,
                stock: Vec::new(),
            }
        ))
    }

    fn mode(&self) -> RunMode {
        RunMode::eq_mode(
            RunMode::eq_mode(self.pre_module_s1.mode(),self.post_module_s1.mode()),
            RunMode::eq_mode(self.pre_module_s2.mode(),self.post_module_s2.mode()))
    }
    
    fn generate(&self) {
        self.pre_module_s1.feedforward(FwdPreS1 {
            msg_gen: self.gen_value,
        });
        self.pre_module_s2.feedforward(FwdPreS2 {
            msg_gen: self.gen_value,
        });
    }

    fn accept(&mut self) {
        let mut acc = self.post_module_s1.ffw_accepted().iter().map(|s| FwdEndProduct {
                msg_gen: s.msg_gen,
                msg_prop: s.msg_prop,
                msg_proc: self.proc_value,
        }).collect::<Vec<FwdEndProduct>>();

        // for demo accepting
        for msg in &acc {
            println!(
                "agent a accept: gen: {}, prop: {}, proc: {}.",
                msg.msg_gen,
                msg.msg_prop,
                msg.msg_proc
            )
        }
        
        self.stock.append(&mut acc);

        let mut acc = self.post_module_s2.ffw_accepted().iter().map(|s| FwdEndProduct {
            msg_gen: s.msg_gen,
            msg_prop: s.msg_prop,
            msg_proc: self.proc_value,
        }).collect::<Vec<FwdEndProduct>>();

        // for demo accepting
        for msg in &acc {
            println!(
                "agent a accept: gen: {}, prop: {}, proc: {}.",
                msg.msg_gen,
                msg.msg_prop,
                msg.msg_proc
            )
        }
        
        self.stock.append(&mut acc);
    }

    pub fn print_values(&self) {
        println!("gen: {}, proc: {}.", self.gen_value, self.proc_value);
    }
    
    pub fn show(&self) {
        for msg in &self.stock {
            println!(
                "agent a buffer: gen: {}, prop: {}, proc: {}.",
                msg.msg_gen,
                msg.msg_prop,
                msg.msg_proc
            )
        }
    }
}
