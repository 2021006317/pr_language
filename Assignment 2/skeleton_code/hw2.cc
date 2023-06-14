#include <string>
#include <iostream>
#include <variant>
#include "box.h"

using std::variant;

struct NUM { 
    int val; 
    NUM(int _val): val(_val){};
};

using Expr = variant<NUM, 
                     box<struct PLUS>, 
                     box<struct MINUS>>;
struct PLUS { 
    Expr e1, e2;
    PLUS(Expr _e1, Expr _e2): e1(_e1), e2(_e2) {}; 
};
struct MINUS { 
    Expr e1, e2; 
    MINUS(Expr _e1, Expr _e2): e1(_e1), e2(_e2) {}; 
};

struct TRUE {};
struct FALSE {};
struct LESS {
    Expr e1, e2; 
    LESS(Expr _e1, Expr _e2): e1(_e1), e2(_e2){}; 
};

using Formula = variant<TRUE, FALSE,
                        box<struct NOT>,
                        box<struct ANDALSO>,
                        box<struct ORELSE>,
                        box<struct IMPLY>,
                        LESS>;
struct NOT { 
    Formula f; 
    NOT(Formula _f): f(_f) {}; 
};
struct ANDALSO { 
    Formula f1, f2; 
    ANDALSO(Formula _f1, Formula _f2): f1(_f1), f2(_f2) {};
};
struct ORELSE { 
    Formula f1, f2; 
    ORELSE(Formula _f1, Formula _f2): f1(_f1), f2(_f2) {};
};
struct IMPLY { 
    Formula f1, f2; 
    IMPLY(Formula _f1, Formula _f2): f1(_f1), f2(_f2) {};
};


int eval_expr(Expr e) {
    // TODO: Students need to implement this function.
    //       That is, to fill out the lambda functions.
    return std::visit(overload {
        [](NUM& n) { 
                },
        [](box<struct PLUS>& p) { 
                },
        [](box<struct MINUS>& m) { 
                },
      }, e);
}

bool eval(Formula f) {
    // TODO: Students need to implement this function.
    //       That is, to fill out the lambda functions.

    return std::visit(overload {
        [](TRUE& t) { 
                },
        [](FALSE& f) { 
                },
        [](box<struct NOT>& n) { 
                  return !eval(n->f);
                },
        [](box<struct ANDALSO>& a) { 
                },
        [](box<struct ORELSE>& a) { 
                },
        [](box<struct IMPLY>& a) { 
                },
        [](LESS& l) { 
                },
      }, f);
}

int main() {
    std::cout << std::boolalpha;

    std::cout << "NOT(LESS(NUM(1), NUM(2))): false=" 
              << eval(NOT(LESS(NUM(1), NUM(2)))) << std::endl;

    std::cout << "IMPLY(LESS(PLUS(NUM(1),NUM(3)), NUM(2)), FALSE()): true=" 
              << eval(IMPLY(LESS(PLUS(NUM(1),NUM(3)), NUM(2)), FALSE())) << std::endl;


    std::cout << "LESS(NUM(1), NUM(2)): true=" << eval(LESS(NUM(1), NUM(2))) << std::endl;

    std::cout << "LESS(NUM(4), NUM(3)): false=" << eval(LESS(NUM(4), NUM(3))) << std::endl;

    std::cout << "ORELSE(TRUE(), FALSE()): true=" << eval(ORELSE(TRUE(), FALSE())) << std::endl;
    std::cout << "ANDALSO(TRUE(), FALSE()): false=" << eval(ANDALSO(TRUE(), FALSE())) << std::endl;
    return 0;
}


