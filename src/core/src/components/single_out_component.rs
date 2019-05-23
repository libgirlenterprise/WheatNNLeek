use std::sync::{Weak, Mutex, Arc};
use crate::operation::{RunMode, RunningSet, Broadcast};
use crate::connectivity::{ActiveAcceptor, PassiveAcceptor};
use crate::components::{OutSet, Linker};

pub struct SingleOutComponent<AA, PA, S>
where AA: ActiveAcceptor<S> + Send + ?Sized,
      PA: 'static + PassiveAcceptor<S> + Send + ?Sized,
      S: Send,
{
    mode: RunMode,
    target: TargetStatus<AA, PA, S>,
}

enum TargetStatus<AA, PA, S>
where AA: ActiveAcceptor<S> + Send + ?Sized,
      PA: 'static + PassiveAcceptor<S> + Send + ?Sized,
      S: Send,
{
    None,
    Active(OutSet<AA, S>),
    Passive(OutSet<PA, S>)
}

impl<AA, PA, S> SingleOutComponent<AA, PA, S>
where AA: ActiveAcceptor<S> + Send + ?Sized,
      PA: 'static + PassiveAcceptor<S> + Send + ?Sized,
      S: Send,
{
    pub fn new() -> SingleOutComponent<AA, PA, S> {
        SingleOutComponent {
            mode: RunMode::Idle,
            target: TargetStatus::None,
        }
    }

    pub fn add_active_target(&mut self, target: Weak<Mutex<AA>>, linker: Arc<Mutex<Linker<S>>>) {
        match &mut self.mode {
            RunMode::Idle => match self.target {
                TargetStatus::None => self.target = TargetStatus::Active(OutSet::new(target, linker)),
                _ => println!("SingleOutComponent already connected!"),
            }
            _ => panic!("SingleOutComponent can only add_conntion when DeviceMode::Idle!"),
        }
    }

    pub fn add_passive_target(&mut self, target: Weak<Mutex<PA>>, linker: Arc<Mutex<Linker<S>>>) {
        match &mut self.mode {
            RunMode::Idle => match self.target {
                TargetStatus::None => self.target = TargetStatus::Passive(OutSet::new(target, linker)),
                _ => println!("SingleOutComponent already connected!"),
            }
            _ => panic!("SingleOutComponent can only add_conntion when DeviceMode::Idle!"),
        }
    }
    
    pub fn mode(&self) -> RunMode {
        self.mode
    }

    pub fn config_mode(&mut self, mode: RunMode) {
        match (mode, &self.mode) {
            (RunMode::Idle, RunMode::Idle) => println!("SingleOutComponent config_mode from Idle to Idle, no effect."),
            (RunMode::Idle, _) => self.config_mode_to(mode),
            (_, RunMode::Idle) => self.config_mode_to(mode),
            (_, _) => panic!("unhandled config_mode: from {:?} to {:?}.", self.mode(), mode),
        }
    }

    fn config_mode_to(&mut self, mode: RunMode) {
        self.mode = mode;
        match &mut self.target {
            TargetStatus::None => (),
            TargetStatus::Active(set) => set.config_mode(mode),
            TargetStatus::Passive(set) => set.config_mode(mode),
        }
    }
    
    pub fn config_channels(&mut self) {
        match &mut self.target {
            TargetStatus::None => (),
            TargetStatus::Active(set) => set.config_channels(),
            TargetStatus::Passive(set) => set.config_channels(),
        }
    }

    pub fn running_passive_devices(&self) -> Vec<RunningSet<Broadcast, ()>> {
        match &self.mode {
            RunMode::Idle => panic!("SingleOutComponent call running_passive_targets when agent Idle!"),
            RunMode::Feedforward => match &self.target {
                TargetStatus::Passive(set) => {
                    let mut v = Vec::with_capacity(1);
                    v.push(RunningSet::<Broadcast, ()>::new(set.target.upgrade().unwrap()));
                    v
                }
                _ => Vec::with_capacity(0)
            }
        }
    }
    
    pub fn feedforward(&self, s: S) {
        match &self.mode {
            RunMode::Feedforward => match &self.target {
                TargetStatus::None => (),
                TargetStatus::Active(set) => set.feedforward(s),
                TargetStatus::Passive(set) => set.feedforward(s),
            }
            _ => panic!("PreAgentmodules1 is not Feedforward when feedforward called!"),
        }
    }
}
