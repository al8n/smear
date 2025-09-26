/*
* The code in this file is modified based on
* https://github.com/facebook/relay/blob/main/compiler/crates/graphql-syntax/src/lexer.rs
*
* Licensed under the MIT license:
*
* Copyright (c) Meta Platforms, Inc. and affiliates.
*
* Permission is hereby granted, free of charge, to any person obtaining a copy
* of this software and associated documentation files (the "Software"), to deal
* in the Software without restriction, including without limitation the rights
* to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
* copies of the Software, and to permit persons to whom the Software is
* furnished to do so, subject to the following conditions:
*
* The above copyright notice and this permission notice shall be included in all
* copies or substantial portions of the Software.
*
* THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
* IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
* FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
* AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
* LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
* OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
* SOFTWARE.
*/

use derive_more::{Deref, DerefMut, From, Into};
use logosky::logos;

pub use block::*;
pub use inline::*;

macro_rules! impl_common_traits {
  ($name:ident::<&$lt:lifetime $ty:ty>::$fn:ident) => {
    impl<$lt> PartialEq<$ty> for $name<&$lt $ty> {
      #[inline(always)]
      fn eq(&self, other: &$ty) -> bool {
        self.$fn().eq(other)
      }
    }

    impl<$lt> PartialEq<$name<&$lt $ty>> for $ty {
      #[inline(always)]
      fn eq(&self, other: &$name<&$lt $ty>) -> bool {
        other.eq(self)
      }
    }

    impl<$lt> PartialOrd<$ty> for $name<&$lt $ty> {
      #[inline(always)]
      fn partial_cmp(&self, other: &$ty) -> Option<core::cmp::Ordering> {
        self.$fn().partial_cmp(other)
      }
    }

    impl<$lt> PartialOrd<$name<&$lt $ty>> for $ty {
      #[inline(always)]
      fn partial_cmp(&self, other: &$name<&$lt $ty>) -> Option<core::cmp::Ordering> {
        other.partial_cmp(self).map(core::cmp::Ordering::reverse)
      }
    }

    impl<$lt> core::borrow::Borrow<$ty> for $name<&$lt $ty> {
      #[inline(always)]
      fn borrow(&self) -> &$ty {
        self
      }
    }

    impl<$lt> AsRef<$ty> for $name<&$lt $ty> {
      #[inline(always)]
      fn as_ref(&self) -> &$ty {
        core::borrow::Borrow::borrow(self)
      }
    }

    impl<$lt> core::ops::Deref for $name<&$lt $ty> {
      type Target = $ty;

      #[inline(always)]
      fn deref(&self) -> &Self::Target {
        self.$fn()
      }
    }

    impl<$lt> From<$name<&$lt $ty>> for &$lt $ty {
      #[inline(always)]
      fn from(s: $name<&$lt $ty>) -> Self {
        s.$fn()
      }
    }
  };
}

mod block;
mod inline;

#[derive(From, Into, Deref, DerefMut)]
pub(super) struct SealedLexer<'a, 'l, T: logos::Logos<'l>>(&'a mut logos::Lexer<'l, T>);
