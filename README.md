# variant-iterator

Typesafe union of iterator types leveraging std::variant. Also includes value_iterator 
which is an iterator and a container combined so that one can "iterate" over a single
value. The point of this is to form a union between a standard library iterator and a 
value_iterator, thus allowing for uniform polymorphic syntax.

I am aware the tests are really terrible; I'll get around to it. This is still just a 
hack at this point.

## Example
```c++
#include <iostream>
#include <variant-iterator.hpp>

using vect_iter = std::vector<int *>::iterator;
using val_iter  = gch::value_iterator<int *>;
using viter     = gch::variant_iterator<vect_iter, val_iter>;

struct base
{
  virtual viter begin (void) = 0;
  virtual viter end   (void) = 0;
}

struct vect_container : base
{
  // ...
  viter begin (void) override { return viter (m_vect.begin ()); }
  viter end   (void) override { return viter (m_vect.end ());   }
private:
  std::vector<int *> m_vect;
}

struct val_container : base
{
  // ...
  viter begin (void) override { return viter (gch::value_begin (m_val)); }
  viter end   (void) override { return viter (gch::value_end   (m_val)); }
private:
  int *m_val;
}

void do_something (base& ref)
{
  for (int *x : ref)
    std::cout << *x << std::endl;
}
```
