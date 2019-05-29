use crossbeam_channel::Receiver as CCReceiver;
use crossbeam_channel::Sender as CCSender;

pub struct TmpContentSimpleFwd<SF: Send> {
    pub ffw_pre: Option<CCSender<SF>>,
    pub ffw_post: Option<CCReceiver<SF>>,
}

pub struct TmpContentStdpFwd<SF: Send, SB: Send> {
    pub ffw_pre: Option<CCSender<SF>>,
    pub ffw_post: Option<CCReceiver<SF>>,
    pub fbw_pre: Option<CCReceiver<SB>>,
    pub fbw_post: Option<CCSender<SB>>,
}    
