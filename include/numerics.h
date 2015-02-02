
#include <utility>

#include "concepts.h"
#include "iterators.h"

namespace alt {

/// 26.7.2 Accumulate [accumulate]
template <class InputIterator, class Terminator,
         class T, class BinaryOperation = std::plus<>>
T accumulate(InputIterator first, Terminator last, T init,
             BinaryOperation binary_op = BinaryOperation())
{
  require_input_range<InputIterator, Terminator>();
  require_copy_construct<T>();
  require_copy_assign<T>();

  for (; first != last; first++)
    init = binary_op(init, *first);
  return std::move(init);
}

/// 26.7.3 Inner product [inner.product]
template <class InputIterator1, class Terminator,
          class InputIterator2,
          class T,
          class BinaryOperation1 = std::plus<>,
          class BinaryOperation2 = std::multiplies<>>
T inner_product(InputIterator1 first1, Terminator last1,
                InputIterator2 first2, T init,
                BinaryOperation1 binary_op1 = BinaryOperation1(),
                BinaryOperation2 binary_op2 = BinaryOperation2())
{
  return  accumulate(iter_prod(first1, first2, binary_op2),
                     fst_end(last1),
                     init,
                     binary_op1);
}

/// 26.7.4 Partial sum [partial.sum]
template<class InputIterator,  class Terminator,
         class OutputIterator, class BinaryOperation = std::plus<>>
OutputIterator partial_sum(InputIterator first, Terminator last,
                           OutputIterator result,
                           BinaryOperation binary_op = BinaryOperation())
{
  if (first == last) return result;

  using value = typename std::iterator_traits<InputIterator>::value_type;
  value acc = *first++;
  *result++ = acc;
  
  for (; first != last; first++) {
    acc = binary_op(std::move(acc), *first);
    *result++ = acc;
  }

  return result;
}

}  // namespace alt
