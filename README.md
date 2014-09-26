Base64 is a group of similar binary-to-text encoding schemes that represent
binary data in an ASCII string format by translating it into a radix-64
representation.  It is specified in RFC 2045.

## Example
A simple encoding and decoding.

```shell
utop # #require "base64";;
utop # let enc = Base64.encode "OCaml rocks!";;
val enc : string = "T0NhbWwgcm9ja3Mh"                                                                         
utop # let plain = Base64.decode enc;;
val plain : string = "OCaml rocks!"                                                                           
utop # 

```
