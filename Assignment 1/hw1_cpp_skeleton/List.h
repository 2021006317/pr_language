#pragma once 

#include <cassert>
#include <memory>
#include <functional> // std::function
#include <iostream> // forEach(), print()

template<class T>
class List
{
    struct Node;
public:
    List() {}
    List(T v) : _head(std::make_shared<Node>(v)) {}
    List(T v, List const& tail) : _head(std::make_shared<Node>(v, tail._head)) {}

    bool isEmpty() const { return !_head; }

    T head() {
        assert(!isEmpty());
        return _head->_val;
    }
    List tail() {
        assert(!isEmpty());
        return List(_head->_next);        
    }
    List cons(T v) const {
        return List(v, *this);
    }
private:
    explicit List (std::shared_ptr<const Node> nodes) : _head(nodes) {}

    std::shared_ptr<const Node> _head;

    struct Node
    {
        Node(T v, std::shared_ptr<const Node> const & tail) : _val(v), _next(tail) {}
        Node(T v) : _val(v), _next(nullptr) {}
        T _val;
        std::shared_ptr<const Node> _next;
    };
};

template<class T>
List<T> cons(T v, List<T> tail) {
    return List(v, tail);
}

template<typename T>
List<T> makeList(const T& arg) {
    return List<T>().cons(arg);
}
template<typename T, typename... Args>
List<T> makeList(const T& arg, const Args&... args) {
    static_assert((std::is_same_v<T, Args> && ...),
            "All arguments must have the same type as T");
    return cons(arg, makeList<T>(args...));
}

template<class T, class F>
void forEach(List<T> lst, F f) 
{
    static_assert(std::is_convertible<F, std::function<void(T)>>::value, 
                 "forEach requires a function type void(T)");
    if (!lst.isEmpty()) {
        f(lst.head());
        forEach(lst.tail(), f);
    }
}

template<class T>
void print(List<T> lst)
{
    forEach(lst, [](T v) 
    {
        std::cout << "(" << v << ") "; 
    });
    std::cout << std::endl;
}

template<class U, class T, class F>
List<U> fmap(F f, List<T> lst)
{
    static_assert(std::is_convertible<F, std::function<U(T)>>::value, 
                 "fmap requires a function type U(T)");
    if (lst.isEmpty()) 
        return List<U>();
    else
        return List<U>(f(lst.head()), fmap<U>(f, lst.tail()));
}

template<class T, class P>
List<T> filter(P p, List<T> lst)
{
    static_assert(std::is_convertible<P, std::function<bool(T)>>::value, 
                 "filter requires a function type bool(T)");
    if (lst.isEmpty())
        return List<T>();
    if (p(lst.head()))
        return List<T>(lst.head(), filter(p, lst.tail()));
    else
        return filter(p, lst.tail());
}

template<class T, class U, class F>
U foldl(F f, U acc, List<T> lst)
{
    static_assert(std::is_convertible<F, std::function<U(U, T)>>::value, 
                 "foldl requires a function type U(U, T)");
    if (lst.isEmpty())
        return acc;
    else
        return foldl(f, f(acc, lst.head()), lst.tail());
}

template<class T, class U, class F>
U foldr(F f, U acc, List<T> lst)
{
    static_assert(std::is_convertible<F, std::function<U(T, U)>>::value, 
                 "foldr requires a function type U(T, U)");
    if (lst.isEmpty())
        return acc;
    else
        return f(lst.head(), foldr(f, acc, lst.tail()));
}

// Extended based on https://bartoszmilewski.com/2013/11/13/functional-data-structures-in-c-lists/
