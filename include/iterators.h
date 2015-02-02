
#pragma once

#include <iterator>
#include "concepts.h"

namespace alt {

struct seq_end { };

template<class Iterator>
struct lifted_iterator :
    std::iterator< typename std::iterator_traits<Iterator>::iterator_category
                 , typename std::iterator_traits<Iterator>::value_type
                 , typename std::iterator_traits<Iterator>::difference_type
                 , typename std::iterator_traits<Iterator>::pointer
                 , typename std::iterator_traits<Iterator>::reference
                 >
{
  using difference_type =
    typename std::iterator_traits<Iterator>::difference_type;

  Iterator iter;

  lifted_iterator(Iterator iter) : iter(std::move(iter)) { }

  lifted_iterator& operator ++ () {
    iter++;
    return *this;
  }

  lifted_iterator operator ++ (int) {
    auto ret = *this;
    ++(*this);
    return ret;
  }

  lifted_iterator& operator += (difference_type d) {
    iter += d;
    return *this;
  }

  lifted_iterator& operator -= (difference_type d) {
    iter -= d;
    return *this;
  }

  typename std::iterator_traits<Iterator>::reference
  operator * () const {
    return *iter;
  }

  Iterator&       base()       { return iter; }
  const Iterator& base() const { return iter; }

  template<class Iterator2>
  bool equal_to(const Iterator2& other) const {
    return base() == other;
  }
};

template<class Iterator>
lifted_iterator<Iterator> lift_input_iter(Iterator iter) {
  return {std::move(iter)};
}

template<class Iterator>
struct iterator_reference :
    std::iterator< typename std::iterator_traits<Iterator>::iterator_category
                 , typename std::iterator_traits<Iterator>::value_type
                 , typename std::iterator_traits<Iterator>::difference_type
                 , typename std::iterator_traits<Iterator>::pointer
                 , typename std::iterator_traits<Iterator>::reference
                 >
{
  Iterator* iter;

  iterator_reference(Iterator& iter) : iter(&iter) { }

  template<class Iterator2>
  bool equal_to(const Iterator2& other) {
    return *iter == other;
  }

  typename std::iterator_traits<Iterator>::reference
  operator * () const {
    return *(*iter);
  }

  iterator_reference& operator ++ () {
    iter++;
  }

  Iterator operator ++ (int) {
    auto ret = *iter;
    (*iter)++;
    return ret;
  }
};

template<class Iterator>
iterator_reference<Iterator> iter_ref(Iterator& iter) {
  return {iter};
}

struct take;

template<class InputIterator>
struct counting_iterator : require_input_iterator<InputIterator>
                         , lifted_iterator<InputIterator>
{
  using Iter = lifted_iterator<InputIterator>;
  using Iter::base;

  using difference_type = typename Iter::difference_type;

  unsigned int count = 0;

  explicit counting_iterator(InputIterator it, difference_type count=0)
      : Iter(it)
      , count(count)
  {
  }

  counting_iterator operator ++ (int) {
    auto ret = *this;
    count++;
    base()++;
    return ret;
  }

  counting_iterator& operator ++ () {
    (*this)++;
    return *this;
  }

  counting_iterator& operator += (difference_type d) {
    base() += d;
    count += d;
    return *this;
  }

  using Iter::equal_to;

  template<class Iterator>
  bool equal_to(const counting_iterator<Iterator>& it) const {
    return it.base() == base() || count == it.count;
  }


  bool equal_to(const take&) const;
};

template<class Iterator>
counting_iterator<Iterator> counter(Iterator it, unsigned int count = 0) {
  return counting_iterator<Iterator>(it, count);
}

struct take {
  size_t count;

  take(int count) : count(count) { }

  template<class Iterator>
  bool equal_to(const counting_iterator<Iterator>& cit) const {
    return cit.count == count;
  }
};

template<class Iterator>
bool counting_iterator<Iterator>::equal_to(const take& t) const {
  return count == t.count;
}

template<class Value>
struct sentinel_iterator :
  std::iterator< std::input_iterator_tag
               , Value
               >
{
  Value value;

  sentinel_iterator(Value value) : value(std::move(value)) { }

  Value&       operator * ()       { return value; }
  const Value& operator * () const { return value; }

  template<class Iterator>
  bool equal_to(const Iterator& it) const {
    return *it == value;
  }
};

template<class X>
sentinel_iterator<X> sentinel(X x) {
  return {std::move(x)};
}

template<class F>
struct generator_iterator :
  std::iterator< std::input_iterator_tag
               , std::result_of_t<F&()>
               >
{
  using value_type = decltype(std::declval<F>()());
  using reference = value_type&;
  using const_reference = const value_type&;

  F f;
  value_type value;

  generator_iterator(F f) : f(std::move(f))
  {
    value = f();
  }

  generator_iterator& operator ++ () {
    value = f();
    return *this;
  }

  generator_iterator operator++(int) {
    auto ret = *this;
    ++(*this);
    return std::move(ret);
  }

  reference       operator * ()       { return value; }
  const_reference operator * () const { return value; }
};

template<class Function>
generator_iterator<Function> generator(Function f) {
  return {f};
}

template<class Predicate>
struct predicate_iterator
{
  Predicate pred;

  predicate_iterator(Predicate pred) : pred(std::move(pred)) { }

  template<class Iterator>
  bool equal_to(const Iterator& it) const {
    return pred(*it);
  }
};

template<class Predicate>
predicate_iterator<Predicate> predicate(Predicate pred) {
  return {std::move(pred)};
}

template<class InputIterator, class TerminalIterator, class Predicate>
struct filter_iterator
  : lifted_iterator<InputIterator>
  , require_input_range<InputIterator, TerminalIterator>

  // FIXME: Checking this predicate seems to trip gcc and clang on seemingly
  // valid code.
  //, require_is_predicate<Predicate, typename std::iterator_traits<InputIterator>::value_type>
{
  using iterator_category = std::input_iterator_tag;
  using value_type = typename std::iterator_traits<InputIterator>::value_type;
  using reference  = typename std::iterator_traits<InputIterator>::reference;

  using Iter = lifted_iterator<InputIterator>;
  using Iter::base;

  TerminalIterator last;
  Predicate pred;

  filter_iterator& operator ++ () {
    base()++;
    while (base() != last && !pred(*base()))
      base()++;
    return *this;
  }

  filter_iterator operator ++ (int) {
    auto ret = *this;
    ++(*this);
    return ret;
  }

  filter_iterator& operator += (typename Iter::difference_type) = delete;
  filter_iterator  operator +  (typename Iter::difference_type) = delete;

  explicit filter_iterator(InputIterator first, TerminalIterator last,
                           Predicate pred)
    : Iter(std::move(first)), last(std::move(last)), pred(std::move(pred))
  {
    if (!this->pred(*base()))
      ++(*this);
  }

  operator bool () const {
    return base() != last;
  }

  using Iter::equal_to;

  bool equal_to(seq_end) const { return base() == last; }
};

template<class InputIterator, class TerminalIterator, class Predicate>
filter_iterator<InputIterator, TerminalIterator, Predicate>
filter(InputIterator first, TerminalIterator last, Predicate pred) {
  return filter_iterator<InputIterator, TerminalIterator, Predicate>{std::move(first), std::move(last), std::move(pred)};
}

// A terminal iterator for joined_iterators, defined below.
template<class Iterator>
struct joined_first_end {
  Iterator end;

  Iterator base() const { return end; }
};

template<class Iterator>
joined_first_end<Iterator> fst_end(Iterator iter) {
  return {iter};
}

template<class Iterator1, class Iterator2>
struct joined_iterators {
  Iterator1 iter1;
  Iterator2 iter2;

  using Traits1 = std::iterator_traits<Iterator1>;
  using Traits2 = std::iterator_traits<Iterator2>;

  // TODO: inherit common iterator tag from iter1 and iter2.
  using iterator_category = typename
    std::common_type<typename Traits1::iterator_category,
                     typename Traits2::iterator_category>::type;

  using value_type = std::pair< typename Traits1::value_type
                              , typename Traits2::value_type >;
  using reference  = std::pair< typename Traits1::reference
                              , typename Traits2::reference >;
  using pointer    = std::pair< typename Traits1::pointer
                              , typename Traits2::pointer >;
  using difference_type = std::pair< typename Traits1::difference_type
                                   , typename Traits2::difference_type >;

  joined_iterators(Iterator1 iter1, Iterator2 iter2) : iter1(iter1)
                                                     , iter2(iter2)
  { }

  joined_iterators& operator ++ () {
    iter1++;
    iter2++;
    return *this;
  }

  joined_iterators operator ++ (int) {
    auto ret = *this;
    ++(*this);
    return ret;
  }

  std::pair<Iterator1, Iterator2>
  base() const {
    return std::make_pair(iter1, iter2);
  }

  template<class Iterator3, class Iterator4>
  bool equal_to(const joined_iterators<Iterator3, Iterator4>& other) const {
    return other.iter1 == iter1 || other.iter2 == iter2;
  }

  template<class Iterator>
  bool equal_to(const joined_first_end<Iterator>& e) const {
    return e.base() == iter1;
  }

  reference operator * () const {
    return reference(*iter1, *iter2);
  }
};

template<class Iterator1, class Iterator2>
joined_iterators<Iterator1, Iterator2> iter_join(Iterator1 first, Iterator2 last) {
  return {std::move(first), std::move(last)};
}

template<class Iterator1, class Iterator2, class BinaryOperation>
struct product_iterators : joined_iterators<Iterator1, Iterator2>
{
  using iter_base = joined_iterators<Iterator1, Iterator2>;

  using iter_base::base;
  using iter_base::iter1;
  using iter_base::iter2;

  using value_type = std::result_of_t<BinaryOperation(
      typename std::iterator_traits<Iterator1>::value_type,
      typename std::iterator_traits<Iterator2>::value_type)>;

  // NOTE: This iterator should contain a value_type and return a reference to
  // it when dereferenced, however this means that when iter == last - 1 and is
  // incremented, op(*iter1, *iter2) will be run on invalid iterators.
  using reference = value_type;
  using pointer = value_type;

  BinaryOperation op;

  product_iterators(Iterator1 iter1, Iterator2 iter2, BinaryOperation op)
    : iter_base(iter1, iter2)
    , op(std::move(op))
  {
  }

  reference       operator * ()       { return op(*iter1, *iter2); }
  const reference operator * () const { return op(*iter1, *iter2); }

};

template<class Iterator1, class Iterator2, class BinaryOperation>
product_iterators<Iterator1, Iterator2, BinaryOperation>
iter_prod(Iterator1 iter1, Iterator2 iter2, BinaryOperation op) {
  return {iter1, iter2, std::move(op)};
}

template<class Iterator1, class Iterator2>
struct either_iterator
{
  Iterator1 iter1;
  Iterator2 iter2;

  either_iterator(Iterator1 iter1, Iterator2 iter2)
    : iter1(iter1), iter2(iter2)
  { }

  template<class Iterator>
  bool equal_to(const Iterator& other) const {
    return iter1 == other || iter2 == other;
  }
};

template<class Iterator1, class Iterator2>
either_iterator<Iterator1, Iterator2> either(Iterator1 iter1, Iterator2 iter2)
{
  return {iter1, iter2};
}

template<class ForwardIterator>
struct adjacent_iterator : lifted_iterator<ForwardIterator>
                         , require_forward_iterator<ForwardIterator>
{
  using base_type = lifted_iterator<ForwardIterator>;

  using traits = std::iterator_traits<ForwardIterator>;
  using _val = typename traits::value_type;
  using _ref = typename traits::reference;
  using _ptr = typename traits::pointer;

  using value_type = std::pair<_val, _val>;
  using reference  = std::pair<_ref, _ref>;
  using pointer    = std::pair<_ptr, _ptr>;

  ForwardIterator next;

  adjacent_iterator(ForwardIterator current) : base_type(current)
                                             , next(++current)
  { }

  using base_type::base;

  template<class Iterator>
  bool equal_to(const Iterator& iter) const {
    return next == iter;
  }

  adjacent_iterator& operator ++ () {
    base() = next++;
    return *this;
  }

  adjacent_iterator operator ++ (int) {
    auto ret = *this;
    ++*this;
    return ret;
  }

  reference operator * () const {
    return reference(*base(), *next);
  }
};

template<class ForwardIterator>
adjacent_iterator<ForwardIterator> adjacent(ForwardIterator iter) {
  return {iter};
}

template<class Iterator, class UnaryOperation>
struct transform_iterator : lifted_iterator<Iterator>
                          , require_input_iterator<Iterator>
{
  using Iter = lifted_iterator<Iterator>;
  using Iter::base;

  using value_type = std::result_of_t<const UnaryOperation&(typename Iter::reference)>;
  using reference = value_type;

  UnaryOperation op;

  transform_iterator(Iterator iter, UnaryOperation op) : Iter(std::move(iter))
                                                       , op(std::move(op))
  { }

  reference operator * () const {
    return op(*base());
  }
};

template<class Iterator, class UnaryOperation>
transform_iterator<Iterator, UnaryOperation>
trans_iter(Iterator iter, UnaryOperation op) {
  return {std::move(iter), std::move(op)};
}

template<class Value>
struct fill_iterator :
    std::iterator< std::input_iterator_tag
                 , Value
                 >
{
  using reference = Value&;

  Value val;

  fill_iterator(Value val) : val(std::move(val)) { }

  const Value& operator * () const { return val; }
  Value&       operator * ()       { return val; }

  fill_iterator& operator ++ ()    { return *this; }
  fill_iterator  operator ++ (int) { return *this; }
};

template<class Value>
fill_iterator<Value> filler(Value val) {
  return {std::move(val)};
}

template<class Iterator>
struct reverse_iterator : lifted_iterator<Iterator>
                        , require_bidirectional_iterator<Iterator>
{
  using traits = std::iterator_traits<Iterator>;

  using reference       = typename traits::reference;
  using pointer         = typename traits::pointer;
  using difference_type = typename traits::difference_type;

  using lifted_iterator<Iterator>::base;

  reverse_iterator() { }

  template<class Iterator2>
  reverse_iterator(Iterator2 iter)
    : lifted_iterator<Iterator>(std::move(iter)) { }

  template<class Iterator2>
  reverse_iterator(const reverse_iterator<Iterator2>& other)
      : lifted_iterator<Iterator>(other.iter)
  { }

  reference operator *  () const { return *std::next(base(), -1); }
  pointer   operator -> () const { return &(*(*this)); }

  reverse_iterator& operator ++ () {
    base()--;
    return *this;
  }

  reverse_iterator operator ++ (int) {
    auto ret = *this;
    ++(*this);
    return ret;
  }

  reverse_iterator& operator -- () {
    base()--;
    return *this;
  }

  reverse_iterator operator -- (int) {
    auto ret = *this;
    --(*this);
    return ret;
  }

  reverse_iterator operator + (difference_type d) const {
    return reverse_iterator(base() - d);
  }

  reverse_iterator operator - (difference_type d) const {
    return reverse_iterator(base() + d);
  }

  template<class Iterator2>
  difference_type
  operator - (const reverse_iterator<Iterator2>& other) const {
    return other.base() - base();
  }

  template<class Iterator2>
  difference_type
  operator - (const Iterator2& other) const {
    return other - base();
  }
};

template<class Iterator>
reverse_iterator<Iterator> make_reverse_iterator(Iterator iter) {
  return reverse_iterator<Iterator>(std::move(iter));
}

template<class Iterator>
struct flip_assign_iterator : lifted_iterator<Iterator>
{
  using base_type = lifted_iterator<Iterator>;
  using base_type::base;

  using traits = std::iterator_traits<Iterator>;

  struct flip_assign {
    typename traits::reference ref;

    template<class T>
    flip_assign& operator = (T& t) {
      t = ref;
      return *this;
    }

    template<class T>
    const flip_assign& operator = (T& t) const {
      t = ref;
      return *this;
    }
  };

  using value_type = typename traits::value_type;
  using reference  = flip_assign;

  flip_assign_iterator(Iterator iter) : base_type(iter) { }

  reference operator * () {
    return flip_assign{*base()};
  }
};

template<class Iterator>
flip_assign_iterator<Iterator> flipped(Iterator iter) {
  return flip_assign_iterator<Iterator>(iter);
}

struct Rank0 {};
struct Rank1 : Rank0 {};

template<class X, class Y>
auto iter_equal(Rank1, const X& x, const Y& y) 
  -> decltype(x.equal_to(y))
{
  return x.equal_to(y);
}

template<class X, class Y>
auto iter_equal(Rank0, const X& x, const Y& y) 
  -> decltype(y.equal_to(x))
{
  return y.equal_to(x);
}

template<class X, class Y>
auto operator == (const X& x, const Y& y) -> decltype(iter_equal(Rank1{}, x,y)) {
  return iter_equal(Rank1{}, x,y);
}

template<class X, class Y>
auto operator != (const X& x, const Y& y) -> decltype(iter_equal(Rank1{}, x,y)) {
  return !iter_equal(Rank1{}, x,y);
}

/// Miscellaneous functional utilities.
template<class Function>
struct unpack_pair_f {
  Function f;

  unpack_pair_f(Function f) : f(std::move(f)) { }

  template<class X, class Y>
  std::result_of_t<const Function&(X&, Y&)>
  operator () (std::pair<X, Y>& p) const {
    return f(p.first, p.second);
  }

  template<class X, class Y>
  std::result_of_t<const Function&(const X&,const  Y&)>
  operator () (const std::pair<X, Y>& p) const {
    return f(p.first, p.second);
  }
};

template<class Function>
unpack_pair_f<Function> unpack_pair(Function f) {
  return {std::move(f)};
}

template<class Function>
struct not_fn_f {
  Function f;

  not_fn_f(Function f) : f(std::move(f)) { }

  template<class...X>
  bool operator () (X&&...x) const {
    return !f(std::forward<X>(x)...);
  }
};

template<class Function>
not_fn_f<Function> not_fn(Function f) {
  return {std::move(f)};
}

template<class Function>
struct flip_f {
  Function f;

  flip_f(Function f) : f(std::move(f)) { }

  template<class X, class...Y>
  decltype(auto) operator () (X&& x, Y&&...y) const {
    return f(std::forward<Y>(y)..., std::forward<X>(x));
  }
};

template<class Function>
flip_f<Function> flip(Function f) {
  return {std::move(f)};
}

/// 24.4.4 Iterator operations [iterator.operations]
using std::advance;

template<class RandomAccessIterator1, class RandomAccessIterator2>
typename std::iterator_traits<RandomAccessIterator1>::difference_type
_distance(RandomAccessIterator1 first, RandomAccessIterator2 last,
         std::random_access_iterator_tag,
         std::random_access_iterator_tag)
{
  return last - first;
}

template<class InputIterator1, class InputIterator2>
typename std::iterator_traits<InputIterator1>::difference_type
_distance(InputIterator1 first, InputIterator2 last,
         std::input_iterator_tag,
         std::input_iterator_tag)
{
  typename std::iterator_traits<InputIterator1>::difference_type
    count = 0;
  while (first != last) {
    first++;
    count++;
  }
  return count;
}

template<class InputIterator1, class InputIterator2>
typename std::iterator_traits<InputIterator1>::difference_type
distance(InputIterator1 first, InputIterator2 last)
{
  return _distance(first, last,
                   typename std::iterator_traits<InputIterator1>::iterator_category(),
                   typename std::iterator_traits<InputIterator2>::iterator_category());
}

using std::next;
using std::prev;

/// mid_point (helper) -- Finds the mid-point of two iterators.
/// TODO: non RA version.
template<class Iterator1, class Iterator2>
Iterator1
mid_point(Iterator1 first, Iterator2 last)
{
  return next(first, distance(first, last) / 2);
}

}  // namespace alt
