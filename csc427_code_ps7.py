

import string
import sys
import os
import argparse
import re

#
# tm-sim.py
#
# author: bjr
# date: 21 mar 2020
# last update: 22 mar 2020
#    16 mar 2021, updated 
#     3 apr 2021, return conventions for accept/reject
#                 verbose_levels reimplemented
#                 character # is not allowed as a tape symbol
#                 for magical reasons, then " is also not allowed
#                 added class method help()
#    15 apr 2021, made multi-tape
#    19 apr 2021, documentation updated
#                 
#
# copyright: Creative Commons. See http://www.cs.miami.edu/home/burt
#

# GRAMMAR for the TM description

# Comments (not shown in BNF) begin with a hash # and continue to the end
#    of the line
# The ident tokens are states
# The symbol tokens are tape symbolss
# The StateTransition semantics is:
#     tape_symbol_read+ tape_symbol_written+ action+ new_state
# The underscore _ is a tape blank:
# The : in a transition rule is the default tape symbol match when there is no
#    exactly matching transition rule; in the target section of the rule it 
#    is the value of the current tape symbol.

# For k tapes, there can only be 0, 1, k-1 or k ':' in the match rule.

# A missing transition is considered a reject, not an error

class TuringMachineMT:
    
    verbose_levels = {'none':0,'verbose':1,'explain':2, 'debug':3}
    result_reasons = ['ok', 'transition missing', 'time limit']

    grammar = """
    M-> (Stanza [emptyline])*
    Stanza-> StartStanza | AcceptStanza | RejectStanza | StateStanza | TapeStanza
    StartStanza-> "start" ":" ident
    AcceptStanza-> "accept" ":" ident ([newline] [indent] ident])*
    RejectStanza-> "reject" ":" ident ([newline] [indent] ident])*
    TapeStanza-> "tapes:" number
    StateStanze-> "state" ":" ident ([newline] [indent] StateTransition)+
    StateTransition-> (symbol|special){k} (symbol|special){k} (action){k} ident
    action-> l|r|n|L|R|N
    symbol-> \w[!$-/]     # note: a tape symbol
    special-> ":"
    ident-> \w+           # note: name of a state

    """

    def __init__(self):
        self.start_state = "" # is an state identifier
        self.accept_states = set() # is a set of state identifiers
        self.reject_states = set() # is a set of state identifiers    
        self.transitions = {} # (state,symbol-tuple):(state,symbol-tuple,action-tuple)
        self.current_state = "" 
        self.step_counter = 0
        self.all_actions = ["r","l","n"]
        self.k = 1           # number of tapes
        self.tapes = [ ['_'] for i in range(self.k)]  
        self.positions = [ 0 for i in range(self.k)]
        self.verbose = 0
        self.result = 0

    def set_start_state(self,state):
        self.start_state = state
        
    def set_k(self,k):
        self.k = k
        self.positions = [ 0 for i in range(self.k)]
        self.tapes = [ ['_'] for i in range(self.k)]

    def set_tape(self,tape_string,k):
        assert k>=0 and k<self.k
        self.tapes[k] =  ['_' if symbol==':' or symbol==' ' else symbol for symbol in tape_string]

    def add_accept_state(self,state):
        self.accept_states.add(state)

    def add_reject_state(self,state):
        self.reject_states.add(state)
    
    def get_current_state(self):
        return self.curent_state

    def add_transition(self,state_from,read_symbols,write_symbols,actions,state_to):
        assert len(read_symbols)==self.k and len(write_symbols)==self.k and len(actions)==self.k
        
        for action in actions:
            if action.lower() not in self.all_actions:
                # return something instead, nobody likes a chatty program
                return "WARNING: unrecognized action." 
        x = (state_from,read_symbols)
        if x in self.transitions:
            # return something instead, nobody likes a chatty program
            return "WARNING: multiple outgoing states not allowed for DFA's."
        self.transitions[x] = (state_to,write_symbols,actions)
        return None

    def restart(self,tape_string):
        self.current_state = self.start_state
        for i in range(self.k):
            self.positions[i] = 0
            self.set_tape('_',i)

        if type(tape_string)==type(''):  # could be an array of strings
            self.set_tape(tape_string,0)

        self.step_counter = 1

    def step_transition(self):
        
        c_s = self.current_state
        reads = tuple(self.tapes[i][self.positions[i]] for i in range(self.k))
        
        wild = False
        while True:
            if (c_s,reads) in self.transitions:
                (new_state, symbols, actions ) = self.transitions[(c_s,reads)]
                break

            # exactly one : is allowed
            for i in range(self.k-1,-1,-1):
                x = tuple(reads[j] if i!=j else ':' for j in range(self.k))
                if (c_s,x) in self.transitions:
                    (new_state, symbols, actions ) = self.transitions[(c_s,x)]
                    wild= True
                    break      
            if wild:
                break
            
            # exactly one non : is allowed
            for i in range(self.k-1,-1,-1):
                x = tuple(':' if i!=j else reads[j] for j in range(self.k))
                if (c_s,x) in self.transitions:
                    (new_state, symbols, actions ) = self.transitions[(c_s,x)]
                    wild = True
                    break     
            if wild:
                break
                
            # all : is allowed
            wild_card = tuple(':' for i in range(self.k))
            if (c_s,wild_card) in self.transitions:
                (new_state, symbols, actions ) = self.transitions[(c_s,wild_card)]
                break 

            # here we implement a rejection of convenience, if there is
            # no transition, tansition target is (:, n, A_REJECT_STATE)
            print(f'line 162: no transition found ({c_s},{reads})')
            self.reason = 1
            return False
        
        # wildcard code
        symbols = tuple(symbols[i] if symbols[i]!=':' else reads[i] for i in range(self.k))

        shout = False
        self.current_state = new_state
        for i in range(self.k):
            self.tapes[i][self.positions[i]] = symbols[i]
            
            if actions[i].lower() != actions[i]:
                shout = True

            if actions[i].lower() == 'l' and self.positions[i]>0:
                self.positions[i] -= 1
            if actions[i].lower() == 'r':
                self.positions[i] += 1
                if self.positions[i]==len(self.tapes[i]):
                    self.tapes[i][self.positions[i]:] = '_'
            if actions[i].lower() == 'n':
                pass
   
        if shout or self.verbose == TuringMachineMT.verbose_levels['explain']:
            self.print_tapes()
        if self.verbose == TuringMachineMT.verbose_levels['debug']:
            print("\t", self.step_counter, "\t", new_state, symbol, action)
            
        self.step_counter += 1
        return True

    def compute_tm(self,tape_string,step_limit=0,verbose='none'):
        self.verbose = TuringMachineMT.verbose_levels[verbose]
        self.result = 0
        self.restart(tape_string)
        if self.verbose == TuringMachineMT.verbose_levels['verbose']:
            self.print_tapes()
        step = 0
            
        stop_states = self.accept_states.union(self.reject_states)
        while self.current_state not in stop_states:
            res = self.step_transition()
            if not res:
                # missing transition is considered a reject
                return False
            step += 1
            if step > step_limit:
                self.result = 2 
                return None

        if self.current_state in self.accept_states:
            return True
        return False

    def get_tapes(self):
        tapes = []
        for t in range(self.k):
            t = self.tapes[t][:]
            # remove trailing blanks; if entirely blank leave one blank
            for i in range(len(t)-1,0,-1):
                if t[i]=='_':
                    del t[i]
                else:
                    break 
            s = ''.join(t)
            tapes.append(s)
        return tapes
        
    def print_tapes(self):
        print(f'{self.current_state}:',end='')
        for i in range(self.k):
            t, p = self.tapes[i], self.positions[i]
            s = ''.join(t[:p] + ['['] + [t[p]] + [']'] + t[p+1:])
            print(f'\t{s}')
    
    def print_tm(self):
        print("\nstart state:\n\t",self.start_state)
        print("accept states:\n\t",self.accept_states)
        print("reject states:\n\t",self.reject_states)
        print("transitions:")
        for t in self.transitions:
            print("\t",t,"->",self.transitions[t])
    
    @classmethod
    def help(cls):
        print('The verbose levels are:')
        for level in cls.verbose_levels:
            print(f'\t{cls.verbose_levels[level]}: {level}')
        print()
        print('The grammar for the Turing Machine description is:')
        print(cls.grammar)
        
        
### end class TuringMachine


class MachineParserMT:

    @staticmethod
    def turing(tm_obj, fa_string):
        """
        Code to parse a Turing Machine description into the Turing Machine object.
        """
        
        fa_array = fa_string.splitlines()
        line_no = 0 
        current_state = ""
        in_state_read = False
        in_accept_read = False
        in_reject_read = False
        state_line_re = '\s+(\w|[!$-/:_])\s+(\w|[!$-/:_])\s+(\w)\s+(\w+)'
        k = 1
        not_seen_a_state_line = True

        for line in fa_array:
            while True:

                # comment lines are fully ignored
                if re.search('^\s*#',line):
                    break

                if re.search('^\s+',line):

                    if in_state_read:
                        m = re.search(state_line_re,line)
                        if m:
                            reads = tuple(m.group(i) for i in range(1,1+k))
                            writes = tuple(m.group(i) for i in range(1+k,1+2*k))
                            actions = tuple(m.group(i) for i in range(1+2*k,1+3*k))
                            to_state = m.group(1+3*k)
                            
                            res = tm_obj.add_transition(current_state,reads,writes,actions,to_state)
                            if res: 
                                print(res, f'line number {line_no}')
                                return False
                            break

                    if in_accept_read:
                        m = re.search('\s+(\w+)',line)
                        if m:
                            tm_obj.add_accept_state(m.group(1))
                            break

                    if in_reject_read:
                        m = re.search('\s+(\w+)',line)
                        if m:
                            tm_obj.add_reject_state(m.group(1))
                            break

                in_state_read = False
                in_accept_read = False
                in_reject_read = False

                # blank lines do end multiline input
                if re.search('^\s*$',line):
                    break ;

                m = re.search('^start:\s*(\w+)',line)
                if m:
                    tm_obj.set_start_state(m.group(1))
                    break

                m = re.search('^accept:\s*(\w+)',line)
                if m:
                    tm_obj.add_accept_state(m.group(1))
                    in_accept_read = True
                    break

                m = re.search('^reject:\s*(\w+)',line)
                if m:
                    tm_obj.add_reject_state(m.group(1))
                    in_reject_read = True
                    break

                m = re.search('^tapes:\s*(\d+)',line)
                if m:
                    assert not_seen_a_state_line
                    k = int(m.group(1))
                    tm_obj.set_k(k)
                    state_line_re = '\s+'
                    for i in range(k):
                        state_line_re += '(\w|[!$-/:_])\s+'
                    for i in range(k):
                        state_line_re += '(\w|[!$-/:_])\s+'
                    for i in range(k):
                        state_line_re += '(\w)\s+'
                    state_line_re += '(\w+)'
                    break

                m = re.search('^state:\s*(\w+)',line)
                if m:
                    not_seen_a_state_line = False
                    in_state_read = True
                    current_state = m.group(1)
                    break

                print(line_no,"warning: unparsable line, dropping: ", line)
                return False
                break

            line_no += 1
        return True

### end class MachineParser


