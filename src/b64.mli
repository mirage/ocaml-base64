(*
 * Copyright (c) 2006-2009 Citrix Systems Inc.
 * Copyright (c) 2010 Thomas Gazagnaire <thomas@gazagnaire.com>
 * Copyright (c) 2014-2016 Anil Madhavapeddy <anil@recoil.org>
 * Copyright (c) 2018 Romain Calascibetta <romain.calascibetta@gmail.com>
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *
 *)

(** Base64 RFC4648 implementation.

    Base64 is a group of similar binary-to-text encoding schemes that represent
    binary data in an ASCII string format by translating it into a radix-64
    representation. It is specified in RFC 4648.

    {e Release %%VERSION%% - %%PKG_HOMEPAGE%%} *)

type alphabet

val default_alphabet : alphabet
(** A 64-character alphabet specifying the regular Base64 alphabet. *)

val uri_safe_alphabet : alphabet
(** A 64-character alphabet specifying the URI- and filename-safe Base64
    alphabet. *)

val make_alphabet : string -> alphabet
val length_alphabet : alphabet -> int

val decode : ?alphabet:alphabet -> string -> string
(** [decode s] decodes the string [s] that is encoded in Base64 format. Will
    leave trailing NULLs on the string, padding it out to a multiple of 3
    characters. [alphabet] defaults to {!default_alphabet}.

    @raise if Invalid_argument [s] is not a valid Base64 string or if we got a
    wrong-padding. *)

val decode_opt : ?alphabet:alphabet -> string -> string option
(** Same as [decode], but returns [None] instead of raising. *)

val decode_result : ?alphabet:alphabet -> string -> (string, [ `Malformed | `Wrong_padding ]) result
(** Same as [decode], but returns an explicit error ([`Malformed] if input is
    malformed or [`Wrong_padding] if input is not padded) if it fails. *)

val encode : ?pad:bool -> ?alphabet:alphabet -> string -> string
(** [encode s] encodes the string [s] into base64. If [pad] is false, no
    trailing padding is added. [pad] defaults to [true], and [alphabet] to
    {!default_alphabet}. *)
