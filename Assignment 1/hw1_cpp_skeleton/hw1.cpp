#include "List.h"


List<int> merge_lists(List<int> xs, List<int> ys) {
    // TODO: Students need to implement this function.
    // You can create a new list and return it as following:
    // List<int> res(an integer value, another list)
    // return res;
    // e.g. List<int> res(10, xs.tail());
    //      return res;
    // Check if a list is empty with isEmpty() function.
    // e.g. if (xs.isEmpty()) { ... }
    

    // You can get the first value in the list xs by
    //   xs.head()
    // and the remaining list can be obtained by
    //   xs.tail()

}

List<int> make_list(int xs[], int size) {
    List<int> res;
    for (int i=size-1; i>=0; i--) {
        res = cons(xs[i], res);
    }
    return res;
}

void test_merge_lists() {
    const int MAX_SIZE = 10;
    const int NUM_ARRAYS = 10;

    int arrays[NUM_ARRAYS][MAX_SIZE] = {{1,3,5,7,9}, {2,4,6,8,10,12},
                                        {1,3}, {1,2,4,6,8,10},
                                        {3}, {1,2,4,6,8,10},
                                        {1}, {1,2,4,6,8,10}, 
                                        {}, {2,4,6,8,10}}; 
    int array_lengths[NUM_ARRAYS] = {5, 6, 2, 6, 1, 6, 1, 6, 0, 5};

    std::cout << "Running test_merge_lists():\n";
    for (int i=0; i<NUM_ARRAYS; i+=2) {
        List<int> xs = make_list(arrays[i], array_lengths[i]);
        List<int> ys = make_list(arrays[i+1], array_lengths[i+1]);

        List<int> res1 = merge_lists(xs, ys);
        print(res1);

        List<int> res2 = merge_lists(ys, xs);
        print(res2);
        std::cout << "----------------\n";
    }
}

List<int> reverse_list(List<int> xs) {
    std::function<List<int>(List<int>,List<int>)> rev_aux;
    rev_aux = [&](List<int> _xs, List<int> _res) {
      // TODO: Students need to implement this function.

      // Function rev_aux is a recursive and nested function.
      // The function needs to be implemented as tail-recursive.
      // Hint: res contains temporary (and final) result.
    };
    List<int> res;
    return rev_aux(xs, res);
}

void test_reverse_lists() {
    const int NUM_ARRAYS = 5;
    const int MAX_SIZE = 10;
    int arrays[NUM_ARRAYS][MAX_SIZE] = {{1,3,5,7,9},
                                        {2,4,6,8,10,12},
                                        {1,3},
                                        {1}, 
                                        {}}; 
    int array_lengths[NUM_ARRAYS] = {5, 6, 2, 1, 0};
    std::cout << "Running test_reverse_lists():\n";
    for (int i=0; i<NUM_ARRAYS; i++) {
        List<int> xs = make_list(arrays[i], array_lengths[i]);
        List<int> res = reverse_list(xs);
        print(res);
        std::cout << "----------------\n";
    }
}

int main() {
    test_merge_lists();
    test_reverse_lists();
    return 0;
}

