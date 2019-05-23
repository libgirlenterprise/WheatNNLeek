extern crate crossbeam_channel;
use crossbeam_channel::Receiver as CCReceiver;
use crossbeam_channel::Sender as CCSender;
use std::sync::{Weak, Mutex};
use crate::supervisor::{DeviceMode, RunMode};

pub struct ConnectionComponent<G: Send + ?Sized, A: Send + ?Sized, R: Send, S: Send> {
    config: DeviceMode<ComponentIdle<G, A>,
                       ComponentFFW<G, A, R, S>> // how about use associated type to avoid R & S? impl trait on trait?
}

impl<G: Send, A: Send, R: Send, S: Send> ConnectionComponent<G, A, R, S>
where G: Send + ?Sized,
      A: Send + ?Sized,
      R: Send,
      S: Send,
{
    pub fn new(pre: Weak<Mutex<G>>, post: Weak<Mutex<A>>) -> ConnectionComponent<G, A, R, S> {
        ConnectionComponent {
            config: DeviceMode::Idle(ComponentIdle::new(pre, post)),
        }
    }

    pub fn mode(&self) -> RunMode {
        RunMode::mode_from_device(&self.config)
    }

    pub fn config_run(&mut self, mode: RunMode) {
        match (mode, &self.config) {
            (RunMode::Idle, _) => println!("config_run for mode Idle, no effect."),
            (RunMode::Feedforward, DeviceMode::Idle(ms)) => {
                // println!("ConnectionComponent config_run to FFW.");
                self.config = DeviceMode::Feedforward(ms.make_ffw());
            },
            (_, _) => panic!("call fn config_run when not DeviceMode::Idle!"),
        }
    }
    
    pub fn config_idle(&mut self) {
        match &self.config {
            DeviceMode::Feedforward(m) => self.config = DeviceMode::Idle(m.make_idle()),
            DeviceMode::Idle(_) => panic!("call fn config_idle when DeviceMode::Idle!"),
        }
    }
    
    pub fn set_pre_channel_ffw(&mut self, pre_channel: Option<CCReceiver<R>>) {
        // println!("connection_component setting pre_channel.");
        match &mut self.config {
            DeviceMode::Feedforward(m) => m.set_pre_channel(pre_channel),
            _ => panic!("call fn set_pre_ffw when not DeviceMode::Feedforward!")
        }
    }

    pub fn set_post_channel_ffw(&mut self, post_channel: Option<CCSender<S>>) {
        // println!("connection_component setting post_channel.");
        match &mut self.config {
            DeviceMode::Feedforward(m) => m.set_post_channel(post_channel),
            _ => panic!("call fn set_post_ffw when not DeviceMode::Feedforward!"),
        }
    }
    
    pub fn import(&self) -> R {
        match &self.config {
            DeviceMode::Feedforward(m) => m.import(),
            DeviceMode::Idle(_) => panic!("call fn import when DeviceMode::Idle!"),
        }
    }

    pub fn export(&self, s: S) {
        match &self.config {
            DeviceMode::Feedforward(m) => m.export(s),
            DeviceMode::Idle(_) => panic!("call fn export when DeviceMode::Idle!"),
        }
    }    
}

pub struct ComponentIdle<G: Send + ?Sized, A: Send + ?Sized> {
    pre: Weak<Mutex<G>>,
    post: Weak<Mutex<A>>,
}

impl<G: Send + ?Sized, A: Send + ?Sized> ComponentIdle<G, A> {
    fn new(pre: Weak<Mutex<G>>, post: Weak<Mutex<A>>) -> ComponentIdle<G, A> {
        ComponentIdle {
            pre,
            post,
        }
    }

    fn make_ffw<R, S>(&self) -> ComponentFFW<G, A, R, S>
    where R: Send,
          S: Send
    {
        // println!("connection ComponentIdle make_ffw.");
        ComponentFFW {
            pre: self.pre.clone(),
            post: self.post.clone(),
            pre_channel: None,
            post_channel: None,
         }
    }
}

pub struct ComponentFFW<G: Send + ?Sized, A: Send + ?Sized, R: Send, S: Send> {
    pre: Weak<Mutex<G>>,
    post: Weak<Mutex<A>>,
    pre_channel: Option<CCReceiver<R>>,
    post_channel: Option<CCSender<S>>,
}

impl<G: Send + ?Sized, A: Send + ?Sized, R: Send, S: Send> ComponentFFW<G, A, R, S> {
    fn make_idle(&self) -> ComponentIdle<G, A> {
        ComponentIdle {
            pre: self.pre.clone(),
            post: self.post.clone(),
        }
    }

    fn set_pre_channel(&mut self, pre_channel: Option<CCReceiver<R>>) {
        // println!("connection_component got pre_channel.");
        self.pre_channel = pre_channel;
    }

    fn set_post_channel(&mut self, post_channel: Option<CCSender<S>>) {
        // println!("connection_component got post_channel.");
        self.post_channel = post_channel;
    }
    
    fn import(&self) -> R {
        match &self.pre_channel {
            None => panic!("FFW connection has no pre_channel when call import!"),
            Some(ch) => ch.recv().unwrap(),
        }
    }

    fn export(&self, s: S) {
        match &self.post_channel {
            None => panic!("FFW connection has no post_channel when call export!"),
            Some(ch) => ch.send(s).unwrap(),
        }
    }
}
