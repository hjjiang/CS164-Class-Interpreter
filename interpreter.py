# PA2: Bytecode interpreter for lambdas and coroutines.  Motivation: stackful
# interpreter cannot implement coroutines.  You are free to modify this file in
# any way you want as long as you do not change the interpreter invokation by
# main.py

import sys, getopt
import pdb
import util

###############################################################################
#                           BYTECODE COMPILER                                 #
###############################################################################
# The bytecode array stores instructions of the main scope; bytecode arrays
# for lambda bodies are nested bytecode arrays, stored in the call instruction

# Bytecode instruction
#   opcode: instuction type
#   ret:    return register
#   reg1:   register of first operand
#   reg2:   register of second operand
#   reg3:   register of third operand
#   args:   list of arguments
#   body:   code
#   callee: function to call
limit = 100000000
sys.setrecursionlimit(limit)

class BcInst(object): # Used for unary instructions
    def __init__(self, opcode, ret):
        self.opcode = opcode
        self.ret = ret
    def __str__(self):
        return '%s\t%s' % (self.opcode, self.ret)
class BcInstBase(BcInst): # Used for most instructions
    def __init__(self, opcode, ret, reg1 = None, reg2 = None, reg3 = None):
        super(BcInstBase, self).__init__(opcode, ret)
        self.reg1 = reg1
        self.reg2 = reg2
        self.reg3 = reg3
    def __str__(self):
        [r1, r2, r3] = map(lambda r: r is None and ' ' or ', ' + str(r), \
                           [self.reg1, self.reg2, self.reg3])
        return '%s\t%s%s%s%s' % (self.opcode, self.ret, r1, r2, r3)
class BcInstSpec(BcInst): # Used for call and lambda
    def __init__(self, opcode, ret, args = None, body = None, reg1 = None):
        super(BcInstSpec, self).__init__(opcode, ret)
        self.reg1 = reg1
        self.body = body
        self.args = args
    def __str__(self):
        ex = self.reg1 is None and self.args or self.reg1
        return '%s\t%s, %s' % (self.opcode, self.ret, ex)

cnt = 0
def bytecode(e):
    def newTemp():
        global cnt
        cnt = cnt + 1
        return '$' + str(cnt)

    def bc(e,t):
        t1, t2, t3 = newTemp(), newTemp(), newTemp()
        # e is a list of statements (body of function or outer level code)
        if type(e) == type([]):
            if len(e) == 0:
                return [BcInst('null',t),BcInst('return',t)]
            else:
                insts = reduce(lambda stmts, s: stmts + bc(s,newTemp()),e[:-1],[])
                insts += bc(e[-1],t)
                insts += [BcInst('return',t)]
                return insts
        # e is an expression or a statement
        if type(e) == type(()):
            # Expressions
            if e[0] == 'null':
                return [BcInst('null',t)]
            if e[0] == 'type':
                return bc(e[1],t1) + [BcInstBase('type',t,t1)]
            if e[0] in ['int-lit', 'fp-lit', 'var']:
                return [BcInstBase('def', t, e[1])]
            if e[0] == 'string-lit':
                return [BcInstBase('string',t,e[1])]
            if e[0] == 'dict-lit':
                return [BcInst('dict',t)]
            if e[0] in ['+', '-','*','/','==','!=','<','<=','>','>=','in','get']:
                return bc(e[1],t1) + bc(e[2],t2) + [BcInstBase(e[0], t, t1, t2)]
            if e[0] == 'len':
                return bc(e[1],t1)+[BcInstBase('len',t,t1)]
            if e[0] == 'lambda':
                return [BcInstSpec('lambda',t,e[1],bc(e[2],t1))]
            if e[0] == 'call':
                args = e[2]
                func = e[1]
                
                arg_temps = [newTemp() for i in range(len(args))]
                ts = list(arg_temps)

                insts = bc(func,t1)

                insts += reduce(lambda code, s: code + bc(s, ts.pop(0)), args, [])
                insts += [BcInstSpec('call',t,arg_temps,None,t1)]
                return insts
            if e[0] == 'ite':
                return bc(e[1],t1) + bc(e[2],t2) + bc(e[3],t3) + [BcInstBase('ite',t,t1,t2,t3)]
            if e[0] == 'coroutine':
                return bc(e[1],t1) + [BcInstBase('coroutine',t,t1)]
            if e[0] == 'resume':
                return bc(e[1],t1) + bc(e[2],t2) + [BcInstBase('resume',t,t1,t2)]
            if e[0] == 'yield':
                return bc(e[1],t1) + [BcInstBase('yield',t,t1)]
            if e[0] == 'input':     
                return [BcInst('input', t)]
            if e[0] == 'ncall':
                # print "ncall: ", e[3]
                return bc(e[1],t1) + bc(e[2],t2) + bc(e[3],t3) + [BcInstBase('ncall',t,t1,t2,t3)]
            

            # Statements
            if e[0] == 'exp':
                return bc(e[1],t)
            if e[0] == 'def':
                return bc(e[2],t) + [BcInstBase('def', e[1], t)]                
            if e[0] == 'asgn':
                return bc(e[2],t) + [BcInstBase('asgn', e[1], t)]
            if e[0] == 'put':        
                return bc(e[1],t1) + bc(e[2],t2) + bc(e[3],t3) + \
                       [BcInstBase('put',t,t1,t2,t3)]
            if e[0] == 'print':
                return bc(e[1],t) + [BcInstBase('print', t)]
            if e[0] == 'error':
                return bc(e[1],t) + [BcInstBase('error', t)]

        raise SyntaxError("Illegal AST node %s " % str(e))
    t = newTemp()
    return t,bc(e,t)

def print_bytecode(p,indent=0):
    for inst in p:
        if inst.opcode != 'lambda':
            print >> sys.stderr, " "*8*indent, inst
        else:
            print >> sys.stderr, " "*8*indent, str(inst)
            print_bytecode(inst.body,indent+1)

###############################################################################
#                           BYTECODE INTERPRETER                              #
###############################################################################
class State:                        # The interpreter state.
    def __init__(self, stmts, env, pc = 0, callstack = None):
        self.stmts = stmts
        self.env = env
        self.pc = pc
        if callstack:
            self.callstack = callstack
        else:
            self.callstack = []

class ProgramEnd(Exception):      # Exception thrown for runtime-errors
    def __init__(self, exit_value):
        self.exit_value = exit_value
      
class Fun:                          # The function: (ret-var, arg list, body)
    def __init__(self, argList, body):
        self.argList = argList
        self.body = body
class FunVal:                       # Function value (a closure): (fun, env)
    def __init__(self, fun, env):
        self.fun = fun
        self.env = env

class Coroutine:
    def __init__(self, lhsVar, state, finished = False):
        self.lhsVar = lhsVar
        self.state = state
        self.finished = finished
    
# Global environment. Persists across inocations of the ExecGlobal function
globEnv = {'__up__':None}

def Exec(stmts):
    """Execute a sequence of statements at the outermost level"""
    return Resume(State(stmts,env)) # return the last statement's value

def ExecFun(closure, args):
    """Execute a function with arguments args."""
    env = dict(zip(closure.fun.argList,args))
    env['__up__'] = closure.env
    return Resume(State(closure.fun.body,env))[1]

def ExecFunByName(stmts,funName,args):
    """Execute stmts and then call function with name 'funName'
    with arguments args. The function definition must be among the statements."""
    env = {'__up__':None}
    Resume(State(stmts,env))
    return ExecFun(env[funName],args)

# def ExecGlobal(ast):
#     ast = desugar(ast)
#     bc = bytecode(ast)[1]
#     # print_bytecode(bc)
#     if (ast != []):
#         if (False):
#             Resume(State(bc,globEnv))
#         else:
#             try:
#                 Resume(State(bc,globEnv))
#             except:
#                 print 'Inter Error'
#                 sys.exit(-1)

def ExecGlobal(ast, bindings = {}): 
    globEnv.update(bindings) 
    globEnv["_G"] = globEnv
    bc = bytecode(desugar(ast))[1] 
    # print_bytecode(bc)
    # tcall_opt(bc) 
    return Resume(State(bc, globEnv))

# This is the main function of the bytecode interpreter.
# Error handling is missing from the skeleton.
def Resume(state):
    """ Arguments represent the state of the coroutine (as well as of the main
    program) stmts: array of bytecodes, pc: index into stmts where the
    execution should (re)start callstack: the stack of callign context of calls
    ppending in teh coroutine env: the current environment. """
    def execGet():
        obj = lookup(inst.reg1)
        key = lookup(inst.reg2)
        def get(obj):
            # print "trying to return obj[key]"
            # print "obj: ", obj
            # print "key: ", key
            try: return obj[key]
            except KeyError:
                if "__mt" not in obj.keys():
                    # print "cannot find __mt."
                    # print "obj: ", obj
                    # print "key: ", key
                    raise ValueError('Cannot find metatable',obj,key)
                else:
                    # if obj is obj["__mt"]["__index"]:
                    #     raise ValueError('Cannot find '+ str(key))
                    obj = obj["__mt"]["__index"]
                    return get(obj)
        ret = get(obj)
        # print "returning: ", ret
        define(inst.ret,ret)

    def lookup(name):
        # print "we're here looking up", name
        def _lookup(name,env):
            try: return env[name]
            except:
                if env['__up__'] is None:
                    raise ValueError('Cannot find ' + str(name))
                return _lookup(name,env['__up__'])
        x = _lookup(name, state.env)
        # print "we found it to be ", x
        return x

    def update(name, val):
        def _update(name,env,val):
            if name in env: env[name] = val
            else: _update(name, env['__up__'],val)
        _update(name,state.env,val)
    def define(name,val):
        state.env[name] = val
    def addScope(parentEnv):
        " create an empty scope and link it to parentEnv "
        return {'__up__': parentEnv}

    def execPrint():
        val = lookup(inst.ret)
        if val == None:
            print 'null'
        else:
            print lookup(inst.ret)

    def execCall(state):

        lhsVar = inst.ret
        func = lookup(inst.reg1)
        if not isinstance(func,FunVal):
            raise TypeError("Trying to call non-lambda")
        fbody = func.fun.body
        fargs = func.fun.argList
        fenv = func.env
        
        aargs = [lookup(a) for a in inst.args]
        state.callstack.append((state.stmts,state.pc,state.env,lhsVar))
        
        state.env = addScope(fenv)
        state.env.update(dict(zip(fargs,aargs)))
        
        state.stmts = fbody
        state.pc = 0        
        
    def execReturn(state):
        if len(state.callstack) == 0:
            raise ProgramEnd((lookup(inst.ret),None,True))
        retVal = lookup(inst.ret)
        (state.stmts,state.pc,state.env,lhsVar) = state.callstack.pop()
        define(lhsVar, retVal)        
        

    def execCoroutine():
        # Implement coroutine.  You must consider what information you
        # need to store for a coroutine.  This is of course dependent on how
        #p you implement resume and yield.
        func = lookup(inst.reg1)
        define(inst.ret, Coroutine(func.fun.argList[0],State(func.fun.body,{'__up__': func.env},0,[])))        

    def execResume():
        # Implement resume.  The second argument to resume is the
        # argument it passes to the coroutine.  You must ensure that you handle
        # the case where you try to resume a coroutine that has already ended.
        # In section, we discussed how you might use the Resume function of
        # this interpreter to handle coroutines.  What do you pass it?  How do
        # you handle its return value?
        coroutine = lookup(inst.reg1)
        if coroutine.finished:
            raise ValueError('Cannot Resume a finished coroutine')
        coroutine.state.env[coroutine.lhsVar] = lookup(inst.reg2)
        result, lhsVar, finished = Resume(coroutine.state)

        coroutine.lhsVar = lhsVar
        coroutine.finished = finished
        define(inst.ret,result)

    def execYield():
        # Implement yield.  The one parameter is an argument passed to
        # whomever resumed the coroutine.  Consider how you can store the
        # information necessary to allow the coroutine to be resumed later and
        # pick up in the right place.  In section, we discussed how to return
        # the information we have here back to whoever resumed this
        # coroutine.
        #print >> sys.stderr, "Yielded: ", lookup(inst.reg1), inst.ret
        raise ProgramEnd((lookup(inst.reg1),inst.ret,False))

    def execNCall():
        nmod = lookup(inst.reg1)
        nfunc = lookup(inst.reg2)
        nargs = lookup(inst.reg3)
        # print "nmod: ", nmod
        # print "nfunc: ", nfunc
        # print "nargs: ", nargs
        glob = util.doImports([nmod])
        execString = 'return ' + nmod + "."+nfunc+"(**n1)"
        # print "execString: ", execString
        func = util.createFunction(name = util.uniqueIdentifier(), 
                                    args = ["n1"], 
                                    code = execString,
                                    env=glob)
        
        define(inst.ret,func(nargs))
    
    def getType():
        v1 = lookup(inst.reg1)
        if isinstance(v1,dict):
            define(inst.ret,'table')
        else:
            define(inst.ret,'other')
    
    def getLength():
        v1 = lookup(inst.reg1)
        length = 0;
        try:
            while v1[length]!= None:
                length +=1
        except KeyError:
            pass
        define(inst.ret,length)

    def addition():
        v1 = lookup(inst.reg1)
        v2 = lookup(inst.reg2)

        if isinstance(v1,basestring) or isinstance(v2,basestring):
            define(inst.ret,unicode(v1) + unicode(v2))
        else:
            define(inst.ret,v1+v2)
    
    def putTable():
        val = lookup(inst.reg3)
        lookup(inst.reg1)[lookup(inst.reg2)] = val
        define(inst.ret,val)

    def boolToInt(boolVal):
        if boolVal:
            return 1
        else:
            return 0

    actions = {
        # represent 164 null with Python's None
        'null':      lambda: define(inst.ret, None),
        'type':      getType,
        'def':       lambda: define(inst.ret,inst.reg1 if type(inst.reg1) == type(1) else lookup(inst.reg1)),
        'string':    lambda: define(inst.ret, inst.reg1),
        'dict':      lambda: define(inst.ret,{}),
        '+':         addition,
        '-':         lambda: define(inst.ret, lookup(inst.reg1) - lookup(inst.reg2)),
        '*':         lambda: define(inst.ret, lookup(inst.reg1) * lookup(inst.reg2)),
        '/':         lambda: define(inst.ret,lookup(inst.reg1)/lookup(inst.reg2)),
        '==':        lambda: define(inst.ret, boolToInt(lookup(inst.reg1) == lookup(inst.reg2))),
        '!=':        lambda: define(inst.ret, boolToInt(lookup(inst.reg1) != lookup(inst.reg2))),
        '<=':        lambda: define(inst.ret, boolToInt(lookup(inst.reg1) <= lookup(inst.reg2))),
        '>=':        lambda: define(inst.ret, boolToInt(lookup(inst.reg1) >= lookup(inst.reg2))),
        '<':         lambda: define(inst.ret, boolToInt(lookup(inst.reg1) < lookup(inst.reg2))),
        '>':         lambda: define(inst.ret, boolToInt(lookup(inst.reg1) > lookup(inst.reg2))),
        'in':        lambda: define(inst.ret, boolToInt(lookup(inst.reg1) in lookup(inst.reg2))),
        # 'get':       lambda: define(inst.ret, lookup(inst.reg1)[lookup(inst.reg2)]),
        'get':       lambda: execGet(),
        'len':       getLength,
        'lambda':    lambda: define(inst.ret, FunVal(Fun(inst.args, inst.body), state.env)),
        'call':      lambda: execCall(state),
        'ite':       lambda: define(inst.ret,lookup(inst.reg2) if lookup(inst.reg1) else lookup(inst.reg3)),
        'coroutine': execCoroutine,
        'resume':    execResume,
        'yield':     execYield,
        'input':     lambda: define(inst.ret,input()),
        'asgn':      lambda: update(inst.ret,lookup(inst.reg1)),
        'put':       putTable,
        'print':     execPrint,
        'return':    lambda: execReturn(state),
        'ncall':     lambda: execNCall(),
    }

    while True:
        inst = state.stmts[state.pc]
        state.pc = state.pc + 1
        try:
            actions[inst.opcode]()
        except ProgramEnd as e:
            #print >> sys.stderr, inst
            #print >> sys.stderr, e.exit_value 
            return e.exit_value
    return NeverReached

def desugar(stmts):
    def desugarExp(e):
        if e[0] in ['+', '-','*','/','==','!=','<=','>=','<','>','in','get']:
            return (e[0],desugarExp(e[1]),desugarExp(e[2]))
        elif (e[0] == 'call'):
            e1 = desugarExp(e[1])
            dArgs = []
            for arg in e[2]:
                dArgs.append(desugarExp(arg))
            return (e[0], e1, dArgs)
        elif (e[0] == 'lambda'):
            return (e[0], e[1], desugarStmts(e[2]))
        elif (e[0] == 'ite'):
            e1 = desugarExp(e[1])
            e2 = desugarExp(e[2])
            e3 = desugarExp(e[3])
            return (e[0], e1, e2, e3)
        elif (e[0] == 'dict-lit'):
            if len(e[1]) == 0:
                return (e[0],)

            inner = []
            inner.append(('def', '#dict', ('dict-lit',)))
            for init in e[1]:
                inner.append(('put', ('var', '#dict'), ('string-lit', init[0]),\
                             desugarExp(init[1])))
            inner.append(('exp', ('var', '#dict')))
            return ('exp', ('call', ('lambda', [], inner), []))
        elif e[0] == '||':
            trueBr = ('lambda',[],[('exp',('int-lit',1))])
            falseBr = ('lambda',[],[('exp',desugarExp(e[2]))])
            return ('call',('ite',desugarExp(e[1]),trueBr, falseBr),[])
        elif e[0] == '&&':
            falseBr = ('lambda',[],[('exp',('int-lit',0))])
            trueBr = ('lambda',[],[('exp',desugarExp(e[2]))])
            return ('call',('ite',desugarExp(e[1]),trueBr, falseBr),[])
        elif e[0] == 'comprehension':
            body = e[1]
            lst = e[3]

            inner = []
            inner.append(('def','#out',('dict-lit',[])))
            inner.append(('def','#i',('int-lit',0)))

            forInner = []
            forInner.append(('put',('var','#out'),('var','#i'),body))
            forInner.append(('asgn','#i',('+',('var','#i'),('int-lit',1))))

            inner.append(('for',e[2],lst,forInner))
            inner.append(('exp',('var','#out')))

            inner = desugarStmts(inner)
            return('call',('lambda', [], inner), [])
        elif e[0] in ['len','coroutine','yield']:
            return (e[0], desugarExp(e[1]))
        elif e[0] == 'resume':
            return (e[0],desugarExp(e[1]),desugarExp(e[2]))
        elif e[0] == 'mcall':
            # argSelf = e[1]
            # if argSelf[0] == 'call':
            getExp = ('get',('var', '#mcTemp'),('string-lit',e[2]))
            args = list()
            args.append(('var','#mcTemp'))
            args.extend(e[3])
            callExp = ('call', desugarExp(getExp),args)
            # print "e[1]: ", e[1]
            defExp = ('def', '#mcTemp',desugarExp(e[1]))
            finExp = list()
            finExp.append(desugarExp(defExp))
            finExp.append(desugarExp(callExp))
            # print ('call',('lambda',[],finExp),[])
            return ('call',('lambda',[],finExp),[])
            # else:
            #     getExp = ('get',e[1], ('string-lit', e[2]))
            #     args = list()
            #     args.append(e[1])
            #     args.extend(e[3])
            #     callExp = ('call', desugarExp(getExp),args)
            #     return desugarExp(callExp)
        elif e[0] == 'ncall':
            modString = ('string-lit',e[1])
            funcString = ('string-lit',e[2])
            dictDesugar = desugarExp(e[3])
            return ('ncall',modString,funcString,dictDesugar)

        else:
            return e
    def desugarStmts(stmts):
        dStmts = []
        for s in stmts:
            if s[0] == 'exp' or s[0] == 'print' or s[0] == 'error':
                s1 = desugarExp(s[1])
                dStmts.append((s[0], s1))
            elif s[0] == 'put':
                dStmts.append((s[0], desugarExp(s[1]), desugarExp(s[2]), desugarExp(s[3])))
            elif s[0] == 'asgn' or s[0] == 'def':           
                s2 = desugarExp(s[2])
                dStmts.append((s[0], s[1], s2))
            elif s[0] == 'fdef':
                s3 = desugarStmts(s[3])
                body = ('lambda', s[2], s3)
                dStmts.append(('def', s[1], body))
            elif s[0] == 'if':
                s1 = desugarExp(s[1])
                s2 = desugarStmts(s[2])
                if s[3]:
                    s3 = desugarStmts(s[3])
                else:
                    s3 = []
                ifBranch = ('lambda',[],s2)
                elseBranch = ('lambda',[],s3)
                dStmts.append(('exp',('call',('ite',s1,ifBranch,elseBranch),[])))
            elif s[0] == "while":
                inner = []
                inner.append(('def', '#while', ('lambda', [], [('if', s[1],s[2] + [('exp', ('call', ('var', '#while'), []))],[])])))
                inner.append(('exp', ('call', ('var', '#while'), [])))
                inner = desugarStmts(inner)
                dStmts.append(('exp', ('call', ('lambda', [], inner), [])))
            elif s[0] == "for":
                inner = []
                inner.append(('def', '#iter', s[2]))
                checkType = ('==',('type',('var','#iter')),('string-lit','table'))
                getIterator = [('asgn', '#iter', ('call', ('var', '_getIterator_'),[('var','#iter')]))]
                inner.append(('if',checkType, getIterator,None))
                inner.append(('def', '#for', ('lambda', [], [('def', s[1], ('call', ('var', '#iter'), [])),
                             ('if', ('!=', ('var', s[1]), ('null',)),
                             s[3] + [('exp', ('call', ('var', '#for'), []))],
                             [])])))
                inner.append(('exp',('call', ('var', '#for'), [])))
                inner = desugarStmts(inner)
                dStmts.append(('exp', ('call', ('lambda', [], inner), [])))
            else:
                dStmts.append(s)
        return dStmts
    return desugarStmts(stmts)