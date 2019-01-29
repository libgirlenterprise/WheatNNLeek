// Copyright (c) 2019 Libgirl
//
// Released under Apache 2.0 license as described in the file LICENSE.txt.

extern crate serde;
extern crate serde_json;

use crate::Index;

#[derive(Clone, Serialize, Deserialize)]
pub struct Population {
    id: usize,
    neuron_ids: Vec<Index>,
    size: usize,
}

impl Population {
    pub fn new(id: usize, ids: &[Index]) -> Population {
        Population {
            id: id,
            neuron_ids: ids.to_owned(),
            size: ids.len(),
        }
    }

    pub fn get_id(&self) -> usize {
        self.id
    }

    pub fn size(&self) -> usize {
        self.size
    }

    pub fn get(&self, i: usize) -> Option<Index> {
        if i < self.size {
            Some(self.neuron_ids[i])
        } else {
            None
        }
    }

    pub fn print_status(&self) {
        println!("{}", serde_json::to_string(self).unwrap());
    }

    pub fn iter(&self) -> PopulationIter {
        PopulationIter { pop: self, curr: 0 }
    }

    pub fn record(&self, _variables: Vec<String>) -> Result<(), String> {
        Ok(())
    }
}

pub struct PopulationIter<'a> {
    pop: &'a Population,
    curr: usize,
}

impl<'a> Iterator for PopulationIter<'a> {
    type Item = Index;

    fn next(&mut self) -> Option<Index> {
        let r = self.pop.get(self.curr);
        self.curr += 1;
        r
    }
}
