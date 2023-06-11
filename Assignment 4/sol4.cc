#include <string>
#include <iostream>
#include <variant>
#include <map>
#include <exception>
#include <stdexcept>
#include "box.h"
#include "List.h"

using std::variant;
using std::string;

// Definition of Expr variants
struct Var {
    string name;
    Var(string _name): name(_name){};
    operator std::string() const { return "Var("+name+")"; }
};
struct Int {
    int val;
    Int(int _val): val(_val){};
    operator std::string() const { return "Int("+std::to_string(val)+")"; }
};
struct AUnit {
    AUnit() {};
    operator std::string() const { return "AUnit()"; }
};
using Expr = variant<Var, 
                     Int,
                     AUnit,
                     box<struct IsAUnit>, 
                     box<struct Add>, 
                     box<struct IfGreater>, 
                     box<struct MLet>,
                     box<struct Fun>, 
                     box<struct Closure>,
                     box<struct APair>,
                     box<struct Fst>,
                     box<struct Snd>,
                     box<struct Call>>; 

template<typename T> bool is(Expr e);

std::string toString(Expr e);

struct Add {
    Expr e1, e2;
    Add(Expr _e1, Expr _e2): e1(_e1), e2(_e2) {}; 
    operator std::string() const { 
        return "Add("+toString(e1)+", "+toString(e2)+")";
    }
};
struct IfGreater {
    Expr e1, e2, e3, e4;
    IfGreater(Expr _e1, Expr _e2, Expr _e3, Expr _e4): e1(_e1), e2(_e2), e3(_e3), e4(_e4) {};
    operator std::string() const {
        return "IfGreater("+toString(e1)+", "+toString(e2)+", "
                           +toString(e3)+", "+toString(e4)+")";
    }
};
struct MLet {
    string varName;
    Expr e1, e2;
    MLet(string _varName, Expr _e1, Expr _e2): varName(_varName), e1(_e1), e2(_e2) {}; 
    operator std::string() const { 
        return "MLet("+varName+", "+toString(e1)+", "+toString(e2)+")";
    }
};
struct APair {
    Expr e1, e2;
    APair(Expr _e1, Expr _e2): e1(_e1), e2(_e2) {};
    operator std::string() const { 
        return "APair("+toString(e1)+", "+toString(e2)+")";
    }
};
struct Fst {
    Expr e;
    Fst(Expr _e): e(_e) {};
    operator std::string() const { return "Fst("+toString(e)+")"; }
};
struct Snd {
    Expr e;
    Snd(Expr _e): e(_e) {};
    operator std::string() const { return "Snd("+toString(e)+")"; }
};
struct IsAUnit {
    Expr e;
    IsAUnit(Expr _e): e(_e) {};
    operator std::string() const { return "IsAUnit("+toString(e)+")"; }
};

struct Fun {
    string funName;
    string argName;
    Expr body;
    Fun(string _f, string _a, Expr _b): funName(_f), argName(_a), body(_b) {}; 
    operator std::string() const { 
        return "Fun("+funName+", "+argName+", "+toString(body)+")";
    }
};
struct Closure {
    std::map<string, Expr> env;
    Fun f;
    Closure(std::map<string, Expr> _env, Fun _f): env(_env), f(_f) {};
    operator std::string() const { 
        return "Closure(env, "+std::string(f)+")";
    }
};
struct Call {
    Expr funExpr, actual;
    Call(Expr _fe, Expr _a): funExpr(_fe), actual(_a) {};
    operator std::string() const { 
        return "Call("+toString(funExpr)+", "+toString(actual)+")";
    }
};
// End of Definition of Expr variants 

// Functions for check variants.
// e.g. is<APair>(e) or is<Int>(Expr(Int(42)))
template<typename T>
bool is(Expr e) { return std::holds_alternative<T>(e); }
template<>
bool is<Closure>(Expr e) { return std::holds_alternative<box<struct Closure>>(e); }
template<>
bool is<IsAUnit>(Expr e) { return std::holds_alternative<box<struct IsAUnit>>(e); }
template<>
bool is<Add>(Expr e) { return std::holds_alternative<box<struct Add>>(e); }
template<>
bool is<IfGreater>(Expr e) { return std::holds_alternative<box<struct IfGreater>>(e); }
template<>
bool is<MLet>(Expr e) { return std::holds_alternative<box<struct MLet>>(e); }
template<>
bool is<Fun>(Expr e) { return std::holds_alternative<box<struct Fun>>(e); }
template<>
bool is<APair>(Expr e) { return std::holds_alternative<box<struct APair>>(e); }
template<>
bool is<Fst>(Expr e) { return std::holds_alternative<box<struct Fst>>(e); }
template<>
bool is<Snd>(Expr e) { return std::holds_alternative<box<struct Snd>>(e); }
template<>
bool is<Call>(Expr e) { return std::holds_alternative<box<struct Call>>(e); }

// Converting Expr to std::string representation.
std::string toString(Expr e) {
    if (is<Int>(e)) {
        return std::get<Int>(e);
    } else if (is<Var>(e)) {
        return std::get<Var>(e);
    } else if (is<AUnit>(e)) {
        return std::get<AUnit>(e);
    } else if (is<IsAUnit>(e)) {
        return *std::get<box<struct IsAUnit>>(e);
    } else if (is<box<struct Add>>(e)) {
        Add add = *std::get<box<struct Add>>(e);
        return add;
    } else if (is<box<struct IfGreater>>(e)) {
        IfGreater ifgt = *std::get<box<struct IfGreater>>(e);
        return ifgt;
    } else if (is<box<struct MLet>>(e)) {
        MLet mlet = *std::get<box<struct MLet>>(e);
        return mlet;
    } else if (is<box<struct Fun>>(e)) {
        Fun fun = *std::get<box<struct Fun>>(e);
        return fun;
    } else if (is<box<struct Closure>>(e)) {
        Closure closure = *std::get<box<struct Closure>>(e);
        return closure;
    } else if (is<box<struct APair>>(e)) {
        return *std::get<box<struct APair>>(e);
    } else if (is<box<struct Fst>>(e)) {
        return *std::get<box<struct Fst>>(e);
    } else if (is<box<struct Snd>>(e)) {
        return *std::get<box<struct Snd>>(e);
    } else if (is<box<struct Call>>(e)) {
        Call call = *std::get<box<struct Call>>(e);
        return call;
    } else {
        throw std::runtime_error("toString(Expr): Unexpected Expr is given!");
    }
}

// Asserts that given Expr is a value in MUPL.
void assertValue(Expr e) {
    if (is<APair>(e)) {
        APair ap = *std::get<box<struct APair>>(e);
        assertValue(ap.e1);
        assertValue(ap.e2);
    } else if (!(is<Int>(e) || 
               is<Closure>(e) ||
               is<AUnit>(e))) {
        throw std::runtime_error(toString(e) + " is not a value!");
    }
}

// Make a new environment by copying from the passed environment.
std::map<string, Expr> makeNewEnvFrom(std::map<string, Expr> fromEnv) {
    std::map<string, Expr> newEnv(fromEnv);
    return newEnv;
}

Expr envlookup(std::map<string, Expr> env, Var v) {
    if (env.count(v.name) == 0) {
        throw std::runtime_error(toString(v)+" is not in the environment");
    } else {
        Expr val = env.at(v.name);
        assertValue(val);
        return val;
    }
}

Expr eval_under_env(Expr e, std::map<string, Expr> env) {
    return std::visit(overload{
        [&](Int& i) { return e;},
        [&](Var& v) {
          Expr val = envlookup(env, v);
          return val;
        },
        [&](box<struct Add>& a) {
          Expr e1 = eval_under_env(a->e1, env);
          Expr e2 = eval_under_env(a->e2, env);
          if (is<Int>(e1) && is<Int>(e2)) {
            Int i1 = std::get<Int>(e1);
            Int i2 = std::get<Int>(e2);
            Expr res(Int(i1.val+i2.val));
            return res;
          } else {
            throw std::runtime_error("Unexpected types for sub-expressions of Add");
          }
        },

        // AUnit() is a MUPL expression (holding no data, much like () in ML). 
        // Notice AUnit() is a MUPL expression, but AUnit is not.
        [&](AUnit& au) { return Expr(AUnit()); },
        // If e1 is a MUPL expression, then IsAUnit(e1) is a MUPL expression (testing for AUnit()).
        [&](box<struct IsAUnit>& isa) { 
            Expr e1 = eval_under_env(isa->e, env);
            if (is<AUnit>(e1)){
                return Expr(Int(1));
            } else {
                return Expr(Int(0));
            }
        },
        // If e1, e2, e3 and e4 are MUPL expressions, then IfGreater(e1, e2, e3, e4) is a MUPL expression.
        // It is a conditional where the result is e3 if e1 is strictly greater than e2 else the result is e4.
        // e1 > e2 ? e3 : e4
        // The results of the evaluation of e1 and e2 should be Int values. Only one of e3 and e4 is evaluated.
        [&](box<struct IfGreater>& ifgt) {
            Expr e1 = eval_under_env(ifgt->e1, env);
            Expr e2 = eval_under_env(ifgt->e2, env);
            if (is<Int>(e1) && is<Int>(e2)) {
                Int i1 = std::get<Int>(e1);
                Int i2 = std::get<Int>(e2);
                if (i1.val > i2.val) {
                    return eval_under_env(ifgt->e3, env);
                } else {
                    return eval_under_env(ifgt->e4, env);
                }
            } else {
                throw std::runtime_error("Unexpected types for sub-expressions of IfGreater");
            }
        }, 
        // If s is a C++ string and e1 and e2 are MUPL expressions, then MLet(s, e1, e2) is a MUPL expression
        // (a let expression where the value resulting e1 is bound to s in the evaluation of e2).
        [&](box<struct MLet>& l) {
            Expr e1 = eval_under_env(l->e1, env);
            std::map<string, Expr> newEnv = makeNewEnvFrom(env);
            newEnv.insert_or_assign(l->varName, e1); // varName = e1;
            return eval_under_env(l->e2, newEnv);
        },
        // If s1 and s2 are C++ strings and e is a MUPL expression, then Fun(s1, s2, e) is a MUPL expression (a function).
        // In e, s1 is bound to the function itself (for recursion) and s2 is bound to the (one) argument.
        // Also, Fun(“”, s2, e) is allowed for anonymous non-recursive function.
        [&](box<struct Fun>& f) {
            return Expr(Closure(env, *std::get<box<struct Fun>>(e)));
        },
        // Closure(env, f) is a MUPL value where f is MUPL function (an expression made from fun) and env is an environment mapping variables to values.
        // Closures do not appear in source programs; they result from evaluating functions. 
        [&](box<struct Closure>& c) {
            return eval_under_env(c->f.body, c->env);
        },
        [&](box<struct APair>& ap) {
            Expr e1 = eval_under_env(ap->e1, env);
            Expr e2 = eval_under_env(ap->e2, env);
            return Expr(APair(e1, e2));
        },
        // If e1 is a MUPL expression, then Fst(e1) is a MUPL expression (getting the first part of a pair).
        [&](box<struct Fst>& fst) {
            Expr e = eval_under_env(fst->e, env);
            if (is<APair>(e)) {
                APair ap = *std::get<box<struct APair>>(e);
                return ap.e1;
            } else {
                throw std::runtime_error("Unexpected type for sub-expression of Fst");
            }
        },
        // If e1 is a MUPL expression, then Snd(e1) is a MUPL expression (getting the second part of a pair).
        [&](box<struct Snd>& snd) { 

            Expr e1 = eval_under_env(snd->e, env);
            if (is<APair>(e1)) {
                APair ap = *std::get<box<struct APair>>(e1);
                return ap.e2;
            } else {
                throw std::runtime_error("Unexpected type for sub-expression of Snd");
            }   
        },
        // If e1 and e2 are MUPL expressions, then Call(e1, e2) is a MUPL expression (a function call).
        [&](box<struct Call>& call) {
            Expr e1 = eval_under_env(call->funExpr, env); // 함수
            Expr e2 = eval_under_env(call->actual, env); // 함수 인자
            if (is<Closure>(e1)) {
                Closure closure = *std::get<box<struct Closure>>(e1);
                std::map<string, Expr> newEnv = makeNewEnvFrom(closure.env);
                newEnv.insert_or_assign(closure.f.argName, e2); // argName에 e2 대입
                if (closure.f.funName != ""){
                    newEnv.insert_or_assign(closure.f.funName, closure); // funName에 closure 대입
                }
                return eval_under_env(closure.f.body, newEnv);
            } else {
                throw std::runtime_error("Unexpected type for sub-expression of Call");
            }
        },
    }, e);
}

Expr eval(Expr e) {
    std::map<string, Expr> env;
    return eval_under_env(e, env);
}

Expr makeIntList(int from, int to) {
    Expr next = AUnit();
    Expr res = AUnit();
    for (int i=to-1; i>=from; i--) {
        Expr tmp = APair(Int(i), next);
        res = tmp;
        next = tmp;
    }
    return res;
}

// e1 is AUnit ? e2 : e3
Expr IfAUnit(Expr e1, Expr e2, Expr e3) {
    Expr res = IfGreater(IsAUnit(e1), Int(0), e2, e3);
    return res;
}

// Problem 1a: Convert List<Expr> to MUPL list
Expr ToMuplList(List<Expr> lst) {
    if (lst.isEmpty()) {
        return Expr(AUnit());
    } else {
        Expr head = lst.head();
        Expr tail = ToMuplList(lst.tail());
        return Expr(APair(head, tail));
    }
}

// Problem 1b: Convert MUPL list to List<Expr>
List<Expr> FromMuplList(Expr muplList) {
    if (is<AUnit>(muplList)) {
        return List<Expr>();
    } else if (is<APair>(muplList)) {
        APair apair = *std::get<box<struct APair>>(muplList);
        Expr head = apair.e1;
        Expr tail = apair.e2;
        List<Expr> rest = FromMuplList(tail);
        return List<Expr>(head, rest);
    } else {
        throw std::runtime_error("FromMuplList: Unexpected MUPL list format");
    }
}

Expr MuplMap() {
	Expr fn = Fun("", "fun_arg",
            Fun("muplrec", "lst",
                IfAUnit(Var("lst"), AUnit(),
                                    APair(Call(Var("fun_arg"), Fst(Var("lst"))),
                                        Call(Var("muplrec"), Snd(Var("lst"))))))
        );
    return fn;
}

Expr MuplMapAddN() {
    Expr map = MuplMap();
    Expr fun = Fun("", "I", Call(map, Fun("", "x", Add(Var("x"), Var("I")))));
    return fun;
}

int main() {
    // Test code for eval()
    std::map<string, Expr> env;
    env.insert_or_assign("a", Expr(Int(40)));

    Expr e = Add(Var("a"), Int(2));
    Expr res = eval_under_env(e, env);
    Int i = std::get<Int>(res);
    std::cout << "e:" << toString(e) << " = " << i.val << std::endl;

    Expr e2 = MLet("a", Int(5), MLet("b", Int(10), Add(Var("a"), Var("b"))));
    res = eval_under_env(e2, env);
    i = std::get<Int>(res);
    std::cout << "e2:" << toString(e2) << " = " << i.val << std::endl;

    res = eval(e2);
    i = std::get<Int>(res);
    std::cout << "eval e2:" << toString(e2) << " = " << i.val << std::endl;


    Expr e3 = Call(Fun("add1", "x", Add(Var("x"), Int(1))), Int(41));
    res = eval_under_env(e3, env);
    i = std::get<Int>(res);
    std::cout << "e3:" << toString(e3) << " = " << i.val << std::endl; 

    Expr e4 = IfGreater(Int(0), Int(1), Int(42), Int(-42));
    res = eval_under_env(e4, env);
    std::cout << "e4:" << toString(e4) << " = " << toString(res) << std::endl; 

    Expr e5 = MLet("a", Int(5), Add(Var("a"), MLet("a", Int(10), Add(Var("a"), Int(1)))));
    res = eval_under_env(e5, env);
    std::cout << "e5:" << toString(e5) << " = " << toString(res) << std::endl;

    Expr e6 = APair(Add(Int(0), Int(10)), APair(Int(1), AUnit()));
    res = eval_under_env(e6, env);
    std::cout << "e6:" << toString(e6) << " = " << toString(res) << std::endl;

    Expr e7 = makeIntList(0, 2);
    std::cout << "e7:" << toString(e7) << " = " << toString(e7) << std::endl;

    Expr e8 = eval(Call(Call(MuplMapAddN(), Int(10)), makeIntList(0, 5)));
    std::cout << "e8:" << toString(e8) << " = " << toString(e8) << std::endl;

    // test for To, From
    List<Expr> myList = makeList(Expr(Int(0)), Expr(Int(1)), Expr(Int(2)));

    Expr e_to = ToMuplList(myList);
    std::cout << "e_to:" << toString(e_to) << " = " << toString(e_to) << std::endl;

    List<Expr> e_from = FromMuplList(e_to);

    std::cout << "e_from: [";
    while (e_from.isEmpty() == false) {
        std::cout << toString(e_from.head());
        e_from = e_from.tail();
        if (e_from.isEmpty() == false)
            std::cout << ", ";
    }
    std::cout << "]" << std::endl;
    
    return 0;
}
