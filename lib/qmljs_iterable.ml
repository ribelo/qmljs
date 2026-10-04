type t = Qmljs_types.value

let object_like value =
  match Qmljs_value.kind value with
  | Object | Array | Function | Promise | Typed_array -> true
  | Undefined | Null | Bool | Int | Float | String | Symbol | Bigint -> false

let protocol_error message =
  Error (Qmljs_error.Structured.of_conversion ("iterator protocol: " ^ message))

let result_object context = function
  | None ->
      let result = Qmljs_object.create context in
      Qmljs_object.set_string result "done" (Qmljs_value.bool context true);
      result
  | Some value ->
      let result = Qmljs_object.create context in
      Qmljs_object.set_string result "done" (Qmljs_value.bool context false);
      Qmljs_object.set_string result "value" value;
      result

let of_step context step =
  let iterator_method =
    Qmljs_function.make context ~name:"Symbol.iterator" ~arity:0 @@ fun _args ->
    let iterator = Qmljs_object.create context in
    let next =
      Qmljs_function.make context ~name:"next" ~arity:0 @@ fun _args ->
      result_object context (step ())
    in
    Qmljs_object.set_string iterator "next" next;
    iterator
  in
  let iterable = Qmljs_object.create context in
  Qmljs_object.define_data_property
    iterable
    ~key:(Qmljs_symbol.iterator context)
    ~value:iterator_method
    ~writable:false
    ~enumerable:false
    ~configurable:true;
  iterable

let next_method value =
  match Qmljs_object.get_string value "next" with
  | Some next when Qmljs_value.is_function next -> Some next
  | Some _ | None -> None

let iterator value =
  match next_method value with
  | Some next -> Ok (value, next)
  | None ->
      let context = Qmljs_types.value_context value in
      let method_ =
        Qmljs_object.get value ~key:(Qmljs_symbol.iterator context)
      in
      if not (Qmljs_value.is_function method_) then
        protocol_error "value is neither iterable nor iterator"
      else
        let iterator = Qmljs_function.call ~this_:value method_ [||] in
        match next_method iterator with
        | Some next -> Ok (iterator, next)
        | None -> protocol_error "iterator does not expose a next function"

let done_ result =
  match Qmljs_object.get_string result "done" with
  | None -> false
  | Some value -> Qmljs_value.to_bool value

let result_value result =
  match Qmljs_object.get_string result "value" with
  | Some value -> value
  | None -> Qmljs_value.undefined (Qmljs_types.value_context result)

let to_list ?(max_steps = 1_000_000) value =
  if max_steps < 0 then invalid_arg "max_steps must be non-negative";
  match
    Qmljs_error.Structured.capture (fun () ->
        let ( let* ) result fn =
          match result with
          | Ok value -> fn value
          | Error _ as error -> error
        in
        let* iterator, next = iterator value in
        let rec loop index acc =
          if index >= max_steps then
            Error
              (Qmljs_error.Structured.of_resource_limit
                 "iterator step limit exceeded")
          else
            let result = Qmljs_function.call ~this_:iterator next [||] in
            if not (object_like result) then
              protocol_error "next returned a non-object"
            else if done_ result then Ok (List.rev acc)
            else loop (index + 1) (result_value result :: acc)
        in
        loop 0 [])
  with
  | Ok result -> result
  | Error _ as error -> error
