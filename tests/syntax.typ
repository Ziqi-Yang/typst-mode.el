// this file is only used for syntax highlight test, and it may not be compilable.
#{
// type test
none, auto, false, true, 1, 1.1, 1e1
1pt, 1mm, 1cm, 1.1in, 1em
1e32deg, 1.32rad
100.0%, 1.1fr
}

= styles<label>
*strong* _emphasized_ `hello` https://typst.app/ @label

\ \aha

- *Content*
  - Text
  - Math
  + Visualize
  + Meta
  
/ Ligature: A merged glyph.
/ 你好.hello: Used as a greeting or to begin a phone conversation.

$ 1 + 1 $
#(

             )	
#if x == 1 {} else if {}

#if x == 1 {

} else {

}

\{ This is not
a code block \}

#let case = "syntax highlight"
the file is for #case test.

#let add(x, y) = x + y
Sum is #add(2, 3).

#emph[Hello] \
#emoji.face \
#"hello".len()
