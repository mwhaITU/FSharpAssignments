module MultiSet
    type MultiSet<'a when 'a : comparison> = MultiSet of Map<'a, uint32>
    let empty = MultiSet Map.empty
    let isEmpty (MultiSet s) = Map.isEmpty s
    let size (MultiSet s) = Map.fold (fun state _ v -> state + v) 0u s
    let contains a (MultiSet s) = Map.containsKey a s
    let numItems a (MultiSet s) = Map.find a s
    let add a n (MultiSet s) = MultiSet (Map.add a n s)
    let addSingle a (MultiSet s) = MultiSet (Map.add a 1u s)
    let remove a n (MultiSet s) = 
        if Map.find a s <= n then
            MultiSet (Map.remove a s)
        else 
            MultiSet (Map.add a ((Map.find a s) - n) s)
    let removeSingle a (MultiSet s) = 
        if Map.containsKey a s then
            MultiSet (Map.add a ((Map.find a s) - 1u) s)
        else
            MultiSet s
    let fold f acc (MultiSet s) = Map.fold f acc s
    let foldBack f (MultiSet s) acc = Map.foldBack f s acc
    let ofList lst = List.fold(fun (s : MultiSet<'a>) x -> addSingle x s) empty lst