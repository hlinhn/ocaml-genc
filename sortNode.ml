open TypeExpr
let sortNode ues =
  let rec collect (ind, ues) ue_ =
    if (List.exists (fun x -> (fst x) == ue_) ues) then
      (ind, ues)
    else
      let ind_to_string i = String.concat "" ["__"; Pervasives.string_of_int i] in
      let (ind', ues') = List.fold_left collect (ind, ues) (ueUpstream ue_) in
      (ind' + 1, (ue_, ind_to_string ind') :: ues') in
  let (_, ues') = List.fold_left collect (0, []) ues in
  List.rev ues'
