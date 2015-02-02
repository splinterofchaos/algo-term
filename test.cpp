
#include <iostream>
#include <list>
#include <vector>

#include <cassert>

#include "include/algorithm.h"
#include "include/numerics.h"

struct NoCopy {
  NoCopy(const NoCopy&) = delete;
  NoCopy(NoCopy&&) { }
  NoCopy() { }
};

struct Fib {
  int last = 0;
  int current = 1;

  int operator() () {
    int next = last + current;
    last = current;
    current = next;
    return current;
  }
};

template<class Seq> void print_seq(const Seq& s) {
  using Value = decltype(*std::begin(s));
  copy(std::begin(s), std::end(s),
       std::ostream_iterator<Value>(std::cout, " "));
  std::cout << std::endl;
}

int main() {
  static_assert(std::is_same<int,
                             typename alt::counting_iterator<int*>::value_type>::value,
                "");

  int arr[] = {1,1,1,2};

  assert(alt::counter(arr) == alt::counter(arr));

  auto equal_to = [](int x) {
    return [=](int y) { return x == y; };
  };

  auto less_than = [](int x) {
    return [=](int y) { return y < x; };
  };

  assert(alt::all_of(arr,  alt::either(std::end(arr),
                                       alt::predicate(equal_to(1))),
                     equal_to(1)));
  assert(!alt::all_of(arr, std::end(arr), equal_to(1)));
  assert(alt::any_of(arr,  std::end(arr), equal_to(2)));
  assert(alt::none_of(arr, std::end(arr), equal_to(0)));

  auto cnt = alt::counter(arr);
  auto two = alt::find(cnt, std::end(arr), 2);
  assert(two == arr+3);
  assert(two.count == 3);

  int odds[] = {1, 3, 5, 7, 9, 11, 13, 15};
  assert(alt::none_of(alt::counter(arr),
                      alt::counter(std::end(arr), 4),
                      equal_to(9)));
  assert(alt::find(alt::counter(arr),
              alt::counter(std::end(arr), 3),
              9) == alt::counter(arr, 3));

  // All odd numbers, before 11, are less than 10.
  assert(alt::all_of(odds, alt::sentinel(11), less_than(10)));

  auto is_vowel = [](char c) { return c == 'a' || c == 'e' || c == 'i'
                                   || c == 'o' || c == 'u'; };
  assert(alt::count_if("hello world", alt::sentinel(0), is_vowel) == 3);

  int fibs[] = {1, 1, 2, 3, 5, 8, 13, 21};
  assert(alt::equal(fibs, std::end(fibs), alt::generator(Fib{})));
  assert(alt::equal(alt::generator(Fib{}), alt::predicate(equal_to(21)), fibs));

  constexpr int n = sizeof fibs / sizeof(int);
  int gen_fibs[n];
  alt::generate_n(gen_fibs, n, Fib{});
  assert(alt::equal(fibs, std::end(fibs), gen_fibs));

  int lowOdds[5];
  alt::copy_n(odds, 5, lowOdds);
  assert(alt::equal(odds, odds+5, lowOdds));

  // Since length(odds) > length(lowOdds), the two arrays are not equal.
  assert(!alt::equal(odds, std::end(odds), lowOdds, std::end(lowOdds)));

  {
    int xs[] = {1,2,3};
    std::list<int> ys = {0,0,0};
    std::vector<int> zs = {1,2,3};
    alt::swap_ranges(std::begin(ys), std::end(ys), std::begin(zs));
    assert(alt::equal(std::begin(xs), std::end(xs), std::begin(ys)));

    // zs == {0,0,0}
    alt::transform(std::begin(zs), std::end(zs), std::begin(zs),
                   [](int x) { return x + 5; });
    // zs == {5,5,5}
    assert(alt::all_of(std::begin(zs), std::end(zs), equal_to(5)));

    alt::transform(std::begin(zs), std::end(zs), std::begin(xs),
                   std::begin(zs), std::plus<>{});
    int zs2[] = {6,7,8};
    assert(alt::equal(std::begin(zs), std::end(zs), std::begin(zs2)));
  }

  {
    int xs[] = {1,0,1,0};
    int ys[] = {1,0,1,0};

    alt::replace(xs, std::end(xs), 0, 1);
    assert(alt::all_of(xs, std::end(xs), equal_to(1)));

    alt::replace_copy(ys, std::end(ys), xs, 1, 0);
    assert(alt::all_of(xs, std::end(xs), equal_to(0)));
  }

  {
    int xs[] = {0,0,0,0,0};
    alt::fill(xs, std::end(xs), 1);
    assert(alt::all_of(xs, std::end(xs), equal_to(1)));

    assert(alt::fill_n(xs, 3, 0) == xs+3);
    assert(alt::all_of(xs, xs+3, equal_to(0)));
    assert(alt::all_of(xs+3, std::end(xs), equal_to(1)));
  }

  {
    int xs[] = {1,0,1,0,1,0};
    auto e = alt::remove(xs, std::end(xs), 0);
    assert(e == xs+3);
    assert(alt::all_of(xs, e, equal_to(1)));

    int ys[] = {1,0,1,0,1,0};
    e = alt::remove_copy(ys, std::end(ys), xs, 0);
    assert(e == xs+3);
    assert(alt::all_of(xs, e, equal_to(0)));
  }

  {
    std::list<int> xs   = {0, 0, 1, 1, 2, 2, 3, 3};
    std::vector<int> ys = {0,1,2,3};
    auto e = alt::unique(std::begin(xs), std::end(xs));
    assert(alt::equal(std::begin(xs), e, std::begin(ys)));
    assert(alt::unique(std::begin(ys), std::end(ys)) == std::end(ys));

    std::vector<int> zs   = {0, 0, 1, 1, 2, 2, 3, 3};
    auto e2 = alt::unique_copy(std::begin(zs), std::end(zs), std::begin(xs));
    assert(alt::equal(std::begin(xs), e2, std::begin(ys)));
  }

  {
    int xs[] = {0,1,2,3,4,5};
    int ys[] = {9,9,9,9,9,9};
    int zs[] = {5,4,3,2,1,0};

    auto e = alt::reverse_copy(xs, std::end(xs), ys);
    assert(e == std::end(ys));
    assert(alt::equal(std::begin(ys), std::end(ys), zs));
    alt::reverse(ys, std::end(ys));
    assert(alt::equal(std::begin(ys), std::end(ys), xs));
  }

  {
    int xs[] = {0,1,2,3};
    int zs[] = {2,3,0,1};

    int* middle = xs + 2;
    assert(alt::rotate(xs, middle, std::end(xs)) == xs + (std::end(xs) - middle));
    assert(alt::equal(xs, std::end(xs), zs));
  }

  {
    int xs[] = {0,0,0,0,1,2,3,4};
    int ys[] = {0,1,0,0,2,3,0,4};

    assert(alt::is_partitioned(xs, std::end(xs), equal_to(0)));
    assert(!alt::is_partitioned(ys, std::end(ys), equal_to(0)));
    assert(alt::partition_point(xs, std::end(xs), equal_to(0)) == xs + 4);

    assert(alt::partition(ys, std::end(ys), equal_to(0)) == ys + 4);

    int zs[] = {0,1,0,0,1,1,0,1};
    int as[4], bs[4];
    assert(alt::partition_copy(zs, std::end(zs), as, bs, equal_to(0))
              == std::make_pair(as+4, bs+4));
    assert(alt::all_of(as, as+4, equal_to(0)));
    assert(alt::none_of(bs, bs+4, equal_to(0)));
  }

  {
    int xs[] = {4,3,2,1,0};
    int ys[] = {0,1,2,3,4};
    int zs[] = {0,1,4,3,2};

    alt::sort(xs, std::end(xs), [](int x, int y) { return x < y; });
    alt::sort(zs, std::end(zs));
    assert(alt::equal(xs, std::end(xs), ys));
    assert(alt::equal(zs, std::end(zs), ys));
  }

  {
    int xs[] = {4,3,2,1,0};
    int ys[] = {0,1,2,3,4};

    alt::partial_sort(xs, xs+3, std::end(xs));
    assert(alt::equal(xs, xs + 3, ys));
    assert(alt::none_of(xs + 3, std::end(xs), less_than(3)));
  }

  {
    int xs[] = {2,4,3,1,0};
    int ys[] = {0,1,2,3,4};
    int zs[] = {0,0,0};

    alt::partial_sort_copy(xs, std::end(xs), zs, std::end(zs));
    assert(alt::equal(zs, std::end(zs), ys));
  }

  {
    int xs[] = {0,1,0,-1};
    int ys[] = {0,1,2,0,-1};
    int zs[] = {0,1,2,3,4};
    assert(alt::is_sorted_until(xs, xs+4) == xs+2);
    assert(alt::is_sorted_until(ys, ys+5) == ys+3);
    assert(alt::is_sorted_until(zs, zs+5) == zs+5);
    assert(!alt::is_sorted(xs, xs+4));
    assert(alt::is_sorted(zs, zs+5));
  }

  {
    int xs[] = {0,1,2,2,3,4};
    assert(alt::lower_bound(xs, std::end(xs), 2) == xs + 2);
    assert(alt::upper_bound(xs, std::end(xs), 2) == xs + 4);
    assert(alt::equal_range(xs, std::end(xs), 2) == std::make_pair(xs+2, xs+4));
    assert(alt::binary_search(xs, std::end(xs), 2));
    assert(alt::binary_search(xs, std::end(xs), 1));
    assert(alt::binary_search(xs, std::end(xs), 3));
    assert(!alt::binary_search(xs, std::end(xs), 5));
  }

  {
    const char* cs = "024";
    std::vector<char> cs2 = {'1', '3'};
    char res[5];

    assert(alt::merge(cs, alt::sentinel(0),
                      std::begin(cs2), std::end(cs2),
                      res) == res + 5);
    assert(alt::equal("01234", alt::sentinel(0), res));

    char cs3[] = "34012\0";
    alt::inplace_merge(cs3, cs3 + 2, alt::sentinel(0));
    assert(alt::equal(cs3, std::end(cs3), "01234\0"));
  }

  {
    assert(alt::includes("hello world", alt::sentinel(0),
                         "world", alt::sentinel(0)));
    assert(!alt::includes("pizza", alt::sentinel(0),
                          "pie", alt::sentinel(0)));
  }

  {
    char buf[5];

    assert(alt::set_union("ace", alt::sentinel(0),
                          "bd", alt::sentinel(0),
                          buf) == buf + 5);
    assert(alt::equal("abcde", alt::sentinel(0), buf));

    assert(alt::set_intersection("acdefg", alt::sentinel(0),
                                 "ccddee", alt::sentinel(0),
                                 buf) == buf + 3);
    assert(alt::equal("cde", alt::sentinel(0), buf));

    assert(alt::set_difference("abcdef", alt::sentinel(0),
                               "bdf", alt::sentinel(0),
                               buf) == buf + 3);
    assert(alt::equal("ace", alt::sentinel(0), buf));

    assert(alt::set_symmetric_difference("abdeg", alt::sentinel(0),
                                         "abcef", alt::sentinel(0),
                                         buf) == buf + 4);
    assert(alt::equal("cdfg", alt::sentinel(0), buf));
  }

  {
    std::vector<int> xs = {0, 2, 4, 6};
    assert(alt::is_heap_until(std::begin(xs), std::end(xs)) == std::begin(xs) + 1);
    alt::make_heap(std::begin(xs), std::end(xs));
    assert(xs[0] == 6);
    assert(alt::is_heap(std::begin(xs), std::end(xs)));
    xs.push_back(5);
    alt::push_heap(std::begin(xs), std::end(xs));
    xs.push_back(1);
    alt::push_heap(std::begin(xs), std::end(xs));
    alt::pop_heap(std::begin(xs), std::end(xs));
    xs.pop_back();
    alt::pop_heap(std::begin(xs), std::end(xs));
    xs.pop_back();
    assert(alt::is_heap(std::begin(xs), std::end(xs)));
  }

  {
    std::vector<int> xs = {0, 1, 2, 3, 4, 5, 6};
    std::vector<int> ys = xs;
    alt::make_heap(std::begin(xs), std::end(xs));
    assert(!alt::equal(std::begin(xs), std::end(xs), std::begin(ys)));
    alt::sort_heap(std::begin(xs), std::end(xs));
    assert(alt::equal(std::begin(xs), std::end(xs), std::begin(ys)));
  }

  {
    const char* str = "0123";
    assert(alt::minmax_element(str, alt::sentinel(0))
              == std::make_pair(str, str + 3));
  }

  {
    assert(alt::lexicographical_compare("Pete",  alt::sentinel(0),
                                        "Peter", alt::sentinel(0)));
    assert(alt::lexicographical_compare("abc", alt::sentinel(0),
                                        "def", alt::sentinel(0)));
  }

  {
    char letters[] = "abc";
    std::string s = letters;

    // "abc" has exactly six permutations.
    assert(alt::next_permutation(letters, letters + 3));   // 1
    assert(alt::next_permutation(letters, letters + 3));   // 2
    assert(alt::next_permutation(letters, letters + 3));   // 3
    assert(alt::next_permutation(letters, letters + 3));   // 4
    assert(alt::next_permutation(letters, letters + 3));   // 5
    assert(!alt::next_permutation(letters, letters + 3));  // 6
    assert(letters == s);
  }

  {
    const char* arr[] = { "hello", " ", "world", nullptr };
    assert(alt::accumulate(arr, alt::sentinel(nullptr), std::string())
              == "hello world");
  }

  {
    // Inner product test: Rock, Paper, Scissors.
    enum Move { ROCK, PAPER, SCISSORS, QUIT };

    // The product function: -1 for playerA wins, 0 for tie, 1 for playerB.
    auto winner = [](Move a, Move b) {
      if (a == b) return 0;
      assert(a != QUIT && b != QUIT);

      if (a == ROCK)     return b == PAPER    ? -1 : 1;
      if (a == PAPER)    return b == SCISSORS ? -1 : 1;
      if (a == SCISSORS) return b == ROCK     ? -1 : 1;
      assert(false);  // unreachable
    };

    Move playerA[] = { ROCK,  ROCK, PAPER, QUIT };
    Move playerB[] = { PAPER, ROCK, SCISSORS };

    assert(alt::inner_product(playerA, alt::sentinel(QUIT),
                              playerB,
                              0,  // initial score
                              std::plus<>(),
                              winner) == -2);
  }

  {
    int xs[] = {1, 2, 3, 4};
    int ys[] = {0, 0, 0, 0};
    int zs[] = {1, 3, 6, 10};

    alt::partial_sum(xs, std::end(xs), ys);
    assert(alt::equal(std::begin(ys), std::end(ys), std::begin(zs)));
  }
}
