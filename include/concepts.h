
#pragma once

/// Concepts
template<class Predicate, class...X>
struct require_is_predicate {
  using pred_result = std::result_of_t<Predicate(X...)>;
  static_assert(std::is_convertible<pred_result, bool>::value,
                "The result of a predicate must be convertible to bool.");
};

template<class X, class Y>
struct require_equality_comparible
    : require_is_predicate<std::equal_to<>, X, Y>
{ };

template<class X>
struct require_copy_construct {
  static void concept(const X& x) {
    X x2(x);
  }
};

template<class X>
struct require_copy_assign {
  static void concept(X x1, const X& x2) {
    x1 = x2;
  }
};

template<class It>
struct require_increment
{
  static constexpr void concept(It it) {
    it++;
    ++it;
  }
};

template<class It>
struct require_decrement
{
  static constexpr void concept(It it) {
    it--;
    --it;
  }
};

template<class It>
struct require_deref
{
  static constexpr auto f(It it) -> decltype(*it);
};

template<class It>
struct require_input_iterator : require_increment<It>
                              , require_deref<It>
{
  using category = typename std::iterator_traits<It>::iterator_category;
  static_assert(std::is_convertible<category, std::input_iterator_tag>::value,
                "");
};

template<class It>
struct require_forward_iterator : require_input_iterator<It>
                                , require_copy_construct<It>
                                , require_copy_assign<It>
{
  using category = typename std::iterator_traits<It>::iterator_category;
  using ref = typename std::iterator_traits<It>::reference;

  static_assert(std::is_convertible<category, std::forward_iterator_tag>::value,
                "");

  static constexpr void concept(ref r) {
    It it;    // default constructible
    r = *it;  // assignable reference type
  }
};

template<class It>
struct require_bidirectional_iterator : require_forward_iterator<It>
                                      , require_decrement<It>
{
  using category = typename std::iterator_traits<It>::iterator_category;
  using ref = typename std::iterator_traits<It>::reference;
  using diff = typename std::iterator_traits<It>::difference_type;

  static_assert(std::is_convertible<category, std::bidirectional_iterator_tag>::value,
                "iterator must be bidirectional");
};

template<class It>
struct require_random_access_iterator 
{
  using category = typename std::iterator_traits<It>::iterator_category;
  using ref = typename std::iterator_traits<It>::reference;
  using diff = typename std::iterator_traits<It>::difference_type;

  static_assert(std::is_convertible<category, std::random_access_iterator_tag>::value,
                "iterator must be random access");

  require_bidirectional_iterator<It> Bi;

  static void concept() {
    It it;
    it += diff(); 
    it -= diff();

    static_assert(std::is_same<ref, decltype(it[diff()])>::value,
                  "`it[i]` must return the reference type");
    static_assert(std::is_same<diff, decltype(it - it)>::value,
                  "`it - it` must return the difference type");
  }
};

template<class It, class Value>
struct require_output_iterator : require_increment<It>
                               , require_deref<It>
{
  using reference = typename std::iterator_traits<It>::reference;
  //static_assert(std::is_assignable<reference, Value>::value,
  //              "must allow `*o = val;`");

  //using category = typename std::iterator_traits<It>::iterator_category;
  //static_assert(std::is_convertible<category, std::output_iterator_tag>::value,
  //              "must be of the 'output iterator' category");
};

template<class It, class Term>
struct require_input_range : require_input_iterator<It>
                           , require_equality_comparible<It, Term>
{ };

template<class It, class Term>
struct require_forward_range : require_forward_iterator<It>
                             , require_equality_comparible<It, Term>
{ };

template<class It1, class It2>
struct require_bidirecitonal_range : require_bidirectional_iterator<It1>
                                   , require_equality_comparible<It1, It2>
{
  // Define as a member since it may cause a duplicate base class error.
  require_bidirectional_iterator<It2> _;
};

template<class It, class Term, class Value>
struct require_output_range : require_output_iterator<It, Value>
                            , require_equality_comparible<It, Term>
{ };

template<class...Iter>
struct require_io_range;

template<class Input, class Output>
struct require_io_range<Input, Output>
    : require_input_iterator<Input>
    , require_output_iterator<Output, typename std::iterator_traits<Input>::reference>
{ };

template<class Input, class Term, class Output>
struct require_io_range<Input, Term, Output>
    : require_input_range<Input, Term>
    , require_output_iterator<Output, typename std::iterator_traits<Input>::reference>
{ };

template<class RandomAccessIterator1, class RandomAccessIterator2>
struct require_random_access_range
{
  require_random_access_iterator<RandomAccessIterator1> ValidIt1;
  require_random_access_iterator<RandomAccessIterator2> ValidIt2;

  void concept(RandomAccessIterator1 iter1, RandomAccessIterator2 iter2) {
    auto diff = iter2 - iter1;
    assert(iter1 + diff == iter2);
  }
};
