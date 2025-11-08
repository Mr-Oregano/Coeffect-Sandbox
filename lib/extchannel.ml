module In_channel = struct
  include Stdlib.In_channel

  let to_seq ic =
    Seq.once
      (Seq.unfold
         (fun channel ->
           match In_channel.input_char channel with
           | Some char -> Some (char, channel)
           | None -> None)
         ic)
end
