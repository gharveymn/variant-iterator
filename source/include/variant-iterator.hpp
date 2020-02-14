/** variant-iterator.hpp
 * Short description here.
 *
 * Copyright © 2019 Gene Harvey
 *
 * This software may be modified and distributed under the terms
 * of the MIT license. See the LICENSE file for details.
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

#ifndef VARIANT_ITERATOR_HPP
#define VARIANT_ITERATOR_HPP

#include <cstdint>
#include <functional>
#include <type_traits>
#include <variant>

namespace gch
{

  struct void_it_t
  {
    static constexpr struct engage_tag { } engage { };
    constexpr explicit void_it_t (engage_tag) noexcept { }
  };

  inline constexpr void_it_t void_it { void_it_t::engage };

  // iterator which doesn't actually do anything.
  // This can be used as an end sentinel in variant_iterator.
  template <typename Value>
  class void_iterator
  {
  public:

    class access_exception : public std::logic_error
    {
    public:
      access_exception (void)
        : std::logic_error ("Tried to access a void iterator.")
      { }
    };

    class modification_exception : public std::logic_error
    {
    public:
      modification_exception (void)
        : std::logic_error ("Tried to modify a void iterator.")
      { }
    };

    using difference_type   = int8_t;
    using value_type        = std::remove_cv_t<Value>;
    using pointer           = Value *;
    using reference         = Value&;
    using iterator_category = std::random_access_iterator_tag;

    void_iterator            (void)                     = default;
    void_iterator            (const void_iterator&)     = default;
    void_iterator            (void_iterator&&) noexcept = default;
    void_iterator& operator= (const void_iterator&)     = default;
    void_iterator& operator= (void_iterator&&) noexcept = default;
    ~void_iterator           (void)                     = default;

    constexpr /* implicit */ void_iterator (void_it_t) noexcept { };

    template <typename NonConst,
      typename = std::enable_if_t<std::is_same_v<std::remove_const_t<Value>, NonConst>>>
    constexpr /* implicit */ void_iterator (const void_iterator<NonConst> it) noexcept { }

    constexpr void_iterator& operator++ (void)
    {
      throw modification_exception { };
    }

    constexpr void_iterator operator++ (int)
    {
      throw modification_exception { };
    }

    constexpr void_iterator& operator-- (void)
    {
      throw modification_exception { };
    }

    constexpr void_iterator operator-- (int)
    {
      throw modification_exception { };
    }

    constexpr void_iterator& operator+= (difference_type n)
    {
      throw modification_exception { };
    }

    [[nodiscard]]
    constexpr void_iterator operator+ (difference_type n) const
    {
      if (n != 0)
        throw std::logic_error { "cannot add non-zero integer to void_iterator type" };
      return { };
    }

    constexpr void_iterator& operator-= (difference_type n)
    {
      throw modification_exception { };
    }

    [[nodiscard]]
    constexpr void_iterator
    operator- (difference_type n) const
    {
      return operator+ (-n);
    }

    [[nodiscard]]
    constexpr difference_type
    operator- (const void_iterator& other) const noexcept
    {
      return 0;
    }

    [[nodiscard]]
    constexpr reference operator[] (difference_type n) const
    {
      throw access_exception { };
    }

    //
    // The below functions will be wrong if other does not refer to the same
    // object. However, this should be fine because it works the same way
    // for std container iterators.
    //

    [[nodiscard]]
    constexpr bool operator< (const void_iterator& other) const noexcept
    {
      return false;
    }

    [[nodiscard]]
    constexpr bool operator> (const void_iterator& other) const noexcept
    {
      return false;
    }

    [[nodiscard]]
    constexpr bool operator<= (const void_iterator& other) const noexcept
    {
      return true;
    }

    [[nodiscard]]
    constexpr bool operator>= (const void_iterator& other) const noexcept
    {
      return true;
    }

    [[nodiscard]]
    constexpr bool operator== (const void_iterator& other) const noexcept
    {
      return true;
    }

    [[nodiscard]]
    constexpr bool operator!= (const void_iterator& other) const noexcept
    {
      return false;
    }

    [[nodiscard]]
    constexpr reference operator* (void) const
    {
      throw access_exception { };
    }

    [[nodiscard]]
    constexpr pointer operator-> (void) const noexcept
    {
      throw access_exception { };
    }

  };

  template <typename Value> [[nodiscard]]
  constexpr void_iterator<Value>
  operator+ (typename void_iterator<Value>::difference_type n, const void_iterator<Value>& it)
  {
    return it + n;
  }

  template <typename Value>
  using const_void_iterator = void_iterator<const Value>;

  template <typename Value>
  using reverse_void_iterator = std::reverse_iterator<void_iterator<Value>>;

  template <typename Value>
  using const_reverse_void_iterator = std::reverse_iterator<const_void_iterator<Value>>;

  template <typename Value> using void_iter   = void_iterator<Value>;
  template <typename Value> using void_citer  = const_void_iterator<Value>;
  template <typename Value> using void_riter  = reverse_void_iterator<Value>;
  template <typename Value> using void_criter = const_reverse_void_iterator<Value>;

  // basically just an iterator-compatible wrapper for a pointer
  // to avoid UB of incrementing a non-associated pointer.
  template <typename Value>
  class value_iterator
  {

    template <typename ...>
    struct not_self : std::true_type { };

    template <typename ...Ts>
    struct not_self<value_iterator<Ts...>> : std::false_type { };

    template <typename T>
    static constexpr bool not_self_v = not_self<T>::value;

  public:

    using difference_type   = int8_t;
    using value_type        = std::remove_cv_t<Value>;
    using pointer           = Value *;
    using reference         = Value&;
    using iterator_category = std::random_access_iterator_tag;

    class dereference_exception : public std::exception
    {
    public:
      [[nodiscard]]
      const char* what (void) const noexcept override
      {
        return "Tried to dereference an incremented iterator.";
      }
    };

    friend value_iterator<std::add_const_t<Value>>;

    value_iterator            (void)                      = default;
    value_iterator            (const value_iterator&)     = default;
    value_iterator            (value_iterator&&) noexcept = default;
    value_iterator& operator= (const value_iterator&)     = default;
    value_iterator& operator= (value_iterator&&) noexcept = default;
    ~value_iterator           (void)                      = default;

    template <typename U = value_type,
      std::enable_if_t<std::conjunction_v<
        not_self<std::remove_reference_t<U>>,
        std::is_constructible<value_type, U&&>,
        std::is_convertible<U&&, value_type>>, bool> = true>
    constexpr /* implicit */ value_iterator (U&& value)
      : m_value (std::forward<U> (value)),
        m_is_end (false)
    { }

    template <typename U = value_type,
      std::enable_if_t<std::conjunction_v<
        not_self<std::remove_reference_t<U>>,
        std::is_constructible<value_type, U&&>,
        std::negation<std::is_convertible<U&&, value_type>>>, bool> = false>
    constexpr explicit value_iterator (U&& value)
      : m_value (std::forward<U> (value)),
        m_is_end (false)
    { }

    template <typename ...Args,
              typename = std::enable_if_t<
                std::is_constructible<value_type, Args&&...>::value>>
    constexpr explicit value_iterator (std::in_place_t, Args&&... args)
      : m_value  (std::forward<Args> (args)...),
        m_is_end (false)
    { }

    // if the value is trivially copyable
    template <typename NonConst,
              typename = std::enable_if_t<std::conjunction_v<
                std::is_same<std::remove_const_t<Value>, NonConst>,
                std::is_trivially_copyable<Value>>>>
    constexpr /* implicit */ value_iterator (const value_iterator<NonConst> it) noexcept
      : m_value  (it.m_value),
        m_is_end (it.m_is_end)
    { }

    // convert from value_iterator to const_value_iterator
    template <typename NonConst,
              typename = std::enable_if_t<std::conjunction_v<
                std::is_same<std::remove_const_t<Value>, NonConst>,
                std::negation<std::is_trivially_copyable<Value>>>>>
    constexpr /* implicit */ value_iterator (const value_iterator<NonConst>& it) noexcept (
          std::is_nothrow_copy_constructible<value_type>::value)
      : m_value  (it.m_value),
        m_is_end (it.m_is_end)
    { }

    // move from value_iterator to const_value_iterator
    template <typename NonConst,
              typename = std::enable_if_t<std::conjunction_v<
                std::is_same<std::remove_const_t<Value>, NonConst>,
                std::negation<std::is_trivially_copyable<Value>>>>>
    constexpr /* implicit */ value_iterator (value_iterator<NonConst>&& it) noexcept (
          std::is_nothrow_move_constructible<value_type>::value)
      : m_value  (std::move (it.m_value)),
        m_is_end (it.m_is_end)
    { }

    constexpr value_iterator& operator++ (void) noexcept
    {
      m_is_end = true;
      return *this;
    }

    constexpr value_iterator operator++ (int) noexcept
    {
      value_iterator save = *this;
      ++*this;
      return save;
    }

    constexpr value_iterator& operator-- (void) noexcept
    {
      m_is_end = false;
      return *this;
    }

    constexpr value_iterator operator-- (int) noexcept
    {
      value_iterator save = *this;
      --*this;
      return save;
    }

    constexpr value_iterator& operator+= (difference_type n) noexcept
    {
      m_is_end = additive_result (n);
      return *this;
    }

    [[nodiscard]]
    constexpr value_iterator operator+ (difference_type n) const noexcept
    {
      return value_iterator (*this) += n;
    }

    constexpr value_iterator& operator-= (difference_type n) noexcept
    {
      return operator+= (-n);
    }

    [[nodiscard]]
    constexpr value_iterator
    operator- (difference_type n) const noexcept
    {
      return operator+ (-n);
    }

    [[nodiscard]]
    constexpr difference_type
    operator- (const value_iterator& other) const noexcept
    {
      return m_is_end - other.m_is_end;
    }

    [[nodiscard]]
    constexpr reference operator[] (difference_type n) const
    {
      if (additive_result (n))
        throw dereference_exception ();
      return m_value;
    }

    //
    // The below functions will be wrong if other does not refer to the same
    // object. However, this should be fine because it works the same way
    // for std container iterators.
    //

    [[nodiscard]]
    constexpr bool operator< (const value_iterator& other) const noexcept
    {
      return ! is_end () && other.is_end ();
    }

    [[nodiscard]]
    constexpr bool operator> (const value_iterator& other) const noexcept
    {
      return is_end () && ! other.is_end ();
    }

    [[nodiscard]]
    constexpr bool operator<= (const value_iterator& other) const noexcept
    {
      return is_end () ? other.is_end () : true;
    }

    [[nodiscard]]
    constexpr bool operator>= (const value_iterator& other) const noexcept
    {
      return is_end () ? true : ! other.is_end ();
    }

    [[nodiscard]]
    constexpr bool operator== (const value_iterator& other) const noexcept
    {
      return is_end () == other.is_end ();
    }

    [[nodiscard]]
    constexpr bool operator!= (const value_iterator& other) const noexcept
    {
      return is_end () != other.is_end ();
    }

    [[nodiscard]]
    constexpr reference operator* (void) const
    {
      if (is_end ())
        throw dereference_exception ();
      return m_value;
    }

    [[nodiscard]]
    constexpr pointer operator-> (void) const noexcept
    {
      return is_end () ? nullptr : std::addressof (m_value);
    }

  private:

    [[nodiscard]]
    constexpr bool is_end (void) const noexcept
    {
      return m_is_end;
    }

    [[nodiscard]]
    constexpr bool additive_result (difference_type n) const noexcept
    {
      return n == 0 ? m_is_end : n > 0;
    }

    mutable value_type m_value;
            bool       m_is_end = true;

  };

  template <typename Value> [[nodiscard]]
  constexpr value_iterator<Value>
  operator+ (typename value_iterator<Value>::difference_type n,
             const value_iterator<Value>& it) noexcept
  {
    return it + n;
  }

  template <typename Value>
  using const_value_iterator = value_iterator<const Value>;

  template <typename Value>
  using reverse_value_iterator = std::reverse_iterator<value_iterator<Value>>;

  template <typename Value>
  using const_reverse_value_iterator = std::reverse_iterator<const_value_iterator<Value>>;

  template <typename Value> using value_iter   = value_iterator<Value>;
  template <typename Value> using value_citer  = const_value_iterator<Value>;
  template <typename Value> using value_riter  = reverse_value_iterator<Value>;
  template <typename Value> using value_criter = const_reverse_value_iterator<Value>;

  // static_assert(std::is_trivially_default_constructible<value_iter<void *>>::value);
  static_assert(std::is_trivially_copyable<value_iter<void *>>::value);
  static_assert(std::is_trivially_destructible<value_iter<void *>>::value);

  static_assert(std::is_trivially_copyable<value_citer<void *>>::value);
  static_assert(std::is_trivially_destructible<value_citer<void *>>::value);

  template <typename T>
  value_iter<typename std::remove_reference_t<T>> value_begin (T&& val)
  {
    return value_iter<typename std::remove_reference_t<T>> (std::forward<T> (val));
  }

  template <typename T>
  value_iter<typename std::remove_reference_t<T>> value_end (T&& val)
  {
    return ++value_begin (std::forward<T> (val));
  }

  template <class... Functors>
  struct overload : Functors...
  {
    using Functors::operator()...;
  };

  template <class... Functors>
  overload (Functors...) -> overload<Functors...>;

  template <typename ...Its>
  class variant_iterator
  {
    template <typename T, typename ...Ts>
    struct all_same : std::conjunction<std::is_same<T, Ts>...>
    { };

    template <std::size_t I, typename T, typename ...Ts>
    struct select
      : select<I - 1, Ts...>
    { };

    template <typename T, typename ...Ts>
    struct select<0, T, Ts...>
    {
      using type = T;
    };

    template <std::size_t I, typename T, typename Head, typename ...Tail>
    struct index : index<I + 1, T, Tail...>
    { };

    template <std::size_t I, typename T, typename ...Rest>
    struct index<I, T, T, Rest...>
      : std::integral_constant<std::size_t, I>
    { };

    template <std::size_t I, typename T>
    struct index<I, T, T>
      : std::integral_constant<std::size_t, I>
    { };

    template <std::size_t I, typename T, typename U>
    struct index<I, T, U>
    { };

    template <typename T, typename = void, typename ...Ts>
    struct is_element_impl : std::false_type
    { };

    template <typename T, typename ...Ts>
    struct is_element_impl<T, std::void_t<typename index<0, T, Ts...>::type>, Ts...>
      : std::true_type
    { };

    template <std::size_t I, typename ...Ts>
    using select_t    = typename select<I, Ts...>::type;

    template <typename T, typename ...Ts>
    using is_element  = is_element_impl<T, void, Ts...>;

    template <typename It, typename = void>
    struct is_iterator : std::false_type
    { };

    template <typename It>
    struct is_iterator<It, std::void_t<std::iterator_traits<It>>>
      : std::true_type
    { };

    template <typename ...Ts>
    static constexpr bool        all_same_v    = all_same<Ts...>::value;

    template <typename T, typename ...Ts>
    static constexpr std::size_t is_element_v  = is_element<T, Ts...>::value;

    template <typename It>
    static constexpr bool        is_iterator_v = is_iterator<It>::value;

    template <typename ...>
    struct not_self : std::true_type { };

    template <typename ...Ts>
    struct not_self<variant_iterator<Ts...>> : std::false_type { };

    template <typename T>
    static constexpr bool not_self_v = not_self<T>::value;

    static_assert (std::conjunction_v<is_iterator<Its>...>,
                   "All template arguments must be iterators.");

    template <typename It>
    using diff_t = typename std::iterator_traits<It>::difference_type;

    template <typename It>
    using value_t = typename std::iterator_traits<It>::value_type;

    template <typename It>
    using pointer_t = typename std::iterator_traits<It>::pointer;

    template <typename It>
    using reference_t = typename std::iterator_traits<It>::reference;

    template <typename It>
    using category_t = typename std::iterator_traits<It>::iterator_category;

  public:

    template <std::size_t I = 0>
    using iterator_type = select_t<I, Its...>;

    static_assert (all_same_v<value_t<Its>...>,
                   "value types must be equal.");

    static_assert (all_same_v<reference_t<Its>...>,
                   "reference types must be equal.");

    static_assert (all_same_v<pointer_t<Its>...>,
                   "pointer types must be equal.");

    using difference_type   = std::common_type_t<diff_t<Its>...>;
    using value_type        = value_t<iterator_type<>>;
    using pointer           = pointer_t<iterator_type<>>;
    using const_pointer     = const value_type *;
    using reference         = reference_t<iterator_type<>>;
    using const_reference   = const value_type&;
    using iterator_category = std::common_type_t<category_t<Its>...>;

    class type_exception : public std::exception
    {
    public:
      [[nodiscard]]
      const char* what (void) const noexcept override
      {
        return "Iterator types are not the same.";
      }
    };

  private:

    struct deduced_tag;

    template <template<typename...> class BinaryOperator, typename Return = deduced_tag>
    struct binary_visitor : BinaryOperator<>
    {
      template <typename It>
      constexpr Return operator() (It&& lhs, It&& rhs) const
      {
        return Return (BinaryOperator<>::operator() (std::forward<It> (lhs),
                                                     std::forward<It> (rhs)));
      }

      template <typename T, typename U,
                typename = std::enable_if_t<! std::is_same<T, U>::value>>
      constexpr Return operator() (T&&, U&&) { throw type_exception (); }
    };

    template <template<typename...> class BinaryOperator>
    struct binary_visitor<BinaryOperator, deduced_tag> : BinaryOperator<>
    {
      using BinaryOperator<>::operator();
      using return_type = std::invoke_result_t<BinaryOperator<>,
                                               iterator_type<>,
                                               iterator_type<>>;

      static_assert (all_same_v<std::invoke_result_t<BinaryOperator<void>,
                                                     Its, Its>...>,
                     "Binary operator return types must be uniform.");

      template <typename T, typename U,
                typename = std::enable_if_t<! std::is_same<T, U>::value>>
      constexpr return_type operator() (T&&, U&&) { throw type_exception (); }
    };

  public:

    variant_iterator            (void)                        = default;
    variant_iterator            (const variant_iterator&)     = default;
    variant_iterator            (variant_iterator&&) noexcept = default;
    variant_iterator& operator= (const variant_iterator&)     = default;
    variant_iterator& operator= (variant_iterator&&) noexcept = default;
    ~variant_iterator           (void)                        = default;

    // value copying initializer
//    template <typename It,
//              typename = std::enable_if_t<is_element_v<It, Its...>>>
//    constexpr explicit variant_iterator (const It& it)
//        : m_variant (it)
//    { }

    template <typename T,
              typename = std::enable_if_t<not_self_v<std::decay_t<T>>>>
    constexpr /* implicit */ variant_iterator (T&& val)
      : m_variant (std::forward<T> (val))
    { }

    constexpr variant_iterator& operator++ (void)
    {
      std::visit ([] (auto&& it) -> void { ++it; }, m_variant);
      return *this;
    }

    constexpr variant_iterator operator++ (int)
    {
      return std::visit ([] (auto&& it) -> variant_iterator
                         { return variant_iterator (it++); }, m_variant);
    }

    constexpr variant_iterator& operator-- (void)
    {
      std::visit ([] (auto&& it) -> void { --it; }, m_variant);
      return *this;
    }

    constexpr variant_iterator operator-- (int)
    {
      return std::visit ([] (auto&& it) -> variant_iterator
                         { return variant_iterator (it--); }, m_variant);
    }

    constexpr variant_iterator& operator+= (difference_type n)
    {
      std::visit ([&n] (auto&& it) { it += n; }, m_variant);
      return *this;
    }

    [[nodiscard]]
    constexpr variant_iterator operator+ (difference_type n) const
    {
      return std::visit ([&n] (auto&& it) -> variant_iterator
                         { return variant_iterator (it + n); }, m_variant);
    }

    [[nodiscard]]
    friend constexpr variant_iterator operator+ (difference_type n, const variant_iterator& var)
    {
      return std::visit ([&n] (auto&& it) -> variant_iterator
                         { return variant_iterator<Its...> (n + it); },
                         var.m_variant);
    }

    constexpr variant_iterator& operator-= (difference_type n)
    {
      std::visit ([&n] (auto&& it) -> void { it -= n; }, m_variant);
      return *this;
    }

    [[nodiscard]]
    constexpr variant_iterator operator- (difference_type n) const
    {
      return std::visit ([&n] (auto&& it) -> variant_iterator
                         { return variant_iterator (it - n); }, m_variant);
    }

    [[nodiscard]]
    constexpr difference_type operator- (const variant_iterator& other) const
    {
      return std::visit (binary_visitor<std::minus, difference_type> { },
                         m_variant, other.m_variant);
    }

    [[nodiscard]]
    constexpr reference operator[] (difference_type n) const
    {
      return std::visit ([&n] (auto&& it) -> reference
                         { return it[n]; },  m_variant);
    }

    [[nodiscard]]
    constexpr bool operator< (const variant_iterator& other) const
    {
      return std::visit (binary_visitor<std::less> { },
                         m_variant, other.m_variant);
    }

    [[nodiscard]]
    constexpr bool operator> (const variant_iterator& other) const
    {
      return std::visit (binary_visitor<std::greater> { },
                         m_variant, other.m_variant);
    }

    [[nodiscard]]
    constexpr bool operator<= (const variant_iterator& other) const
    {
      return std::visit (binary_visitor<std::less_equal> { },
                         m_variant, other.m_variant);
    }

    [[nodiscard]]
    constexpr bool operator>= (const variant_iterator& other) const
    {
      return std::visit (binary_visitor<std::greater_equal> { },
                         m_variant, other.m_variant);
    }

    [[nodiscard]]
    constexpr bool operator== (const variant_iterator& other) const
    {
      return std::visit (binary_visitor<std::equal_to> { },
                         m_variant, other.m_variant);
    }

    [[nodiscard]]
    constexpr bool operator!= (const variant_iterator& other) const
    {
      return std::visit (binary_visitor<std::not_equal_to> { },
                         m_variant, other.m_variant);
    }

    [[nodiscard]]
    constexpr reference operator* (void) const
    {
      return std::visit ([] (auto&& it) -> reference
                         { return it.operator* (); }, m_variant);
    }

    [[nodiscard]]
    constexpr pointer operator-> (void) const
    {
      return std::visit ([] (auto&& it) -> pointer
                         { return it.operator-> (); }, m_variant);
    }

  private:

    std::variant<Its...> m_variant;

  };
}

#endif
