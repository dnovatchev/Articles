declare namespace f = "http://www.w3.org/2005/xpath-functions-2025";
declare namespace gn = "http://www.w3.org/2005/xpath-functions-2025/generator";
declare function gn:to-array($gen as f:generator) as array(*)
{
   while-do( [$gen, []],
          function( $in-out-args) 
          { $in-out-args(1)?initialized and not($in-out-args(1)?end-reached) },                 
          function($in-out-args) 
          { array{$in-out-args(1) =?> move-next(), 
                  array:append($in-out-args(2), $in-out-args(1) =?> get-current())
                 } 
           }         
 ) (2)
};

declare function gn:value($gen as f:generator) {$gen =?> get-current()};
declare function gn:next($gen as f:generator) {$gen =?> move-next()};

declare function gn:take($gen as f:generator, $n as xs:integer) as f:generator
{
  let $gen := if(not($gen?initialized)) then $gen =?> move-next()
                else $gen
   return
     if($gen?end-reached or $n le 0) then gn:empty-generator()
      else
        let $current := $gen =?> get-current(),
            $newResultGen := map:put($gen, "get-current",   fn($this as f:generator){$current}),
            $nextGen := $gen =?> move-next()
         return
           if($nextGen?end-reached) then $newResultGen
             else
               let
                   $newResultGen2 :=  map:put($newResultGen, "move-next",   fn($this as f:generator) {gn:take($nextGen, $n -1)}) 
                 return
                   $newResultGen2  
};

declare function gn:take-while($gen as f:generator, $pred as function(item()*) as xs:boolean) as f:generator
{
  let $gen := if(not($gen?initialized)) then $gen =?> move-next()
                else $gen
   return
     if($gen?end-reached) then gn:empty-generator()
      else      
        let $current := $gen =?> get-current()
          return
            if(not($pred($current))) then gn:empty-generator()
            else
              let $newResultGen := map:put($gen, "get-current",   fn($this as f:generator){$current}),
                  $nextGen := $gen =?> move-next()
               return
                  if($nextGen?end-reached) then $newResultGen
                  else
                    let $newResultGen2 :=  map:put($newResultGen, "move-next",   fn($this as f:generator) {gn:take-while($nextGen, $pred)}) 
                     return $newResultGen2    
};

declare function gn:skip-strict($gen as f:generator, $n as xs:nonNegativeInteger, $issueErrorOnEmpty as xs:boolean) as f:generator
{
  if($n eq 0) then $gen
    else if($gen?end-reached) 
           then if($issueErrorOnEmpty)
                 then error((), "Input Generator too-short") 
                 else gn:empty-generator()
    else 
      let $gen := if(not($gen?initialized)) then $gen =?> move-next()
                   else $gen
        return
          if(not($gen?end-reached)) then gn:skip-strict($gen =?> move-next(), $n -1, $issueErrorOnEmpty)
            else gn:empty-generator()    
};

declare function gn:skip($gen as f:generator, $n as xs:nonNegativeInteger) as f:generator
{
  gn:skip-strict($gen, $n, false())
};
declare function gn:skip-while($gen as f:generator, $pred as function(item()*) as xs:boolean) as f:generator
{
  let $gen := if(not($gen?initialized)) then $gen =?> move-next()
                else $gen
   return
     if($gen?end-reached) then gn:empty-generator()
      else
        let $current := $gen =?> get-current()
         return
           if(not($pred($current))) then $gen
            else gn:skip-while($gen =?> move-next(), $pred)  
};

declare function gn:subrange($gen as f:generator, $m as xs:positiveInteger, $n as xs:positiveInteger) as f:generator
{
 gn:take(gn:skip($gen, $m - 1), $n - $m + 1)  
};

declare function gn:some($gen as f:generator) as xs:boolean
{
 $gen?initialized and not($gen?end-reached)  
};

declare function gn:some-where($gen as f:generator, $pred) as xs:boolean
{
 gn:some(gn:filter($gen, $pred))
};

declare function gn:first-where($gen as f:generator, $pred as fn(item()*) as xs:boolean) as item()*
{
   $gen => gn:skip-while(fn($x as item()*){not($pred($x))}) => gn:head()
 (: gn:head(gn:filter($gen, $pred)) :)
};

declare function gn:chunk($gen as f:generator, $size as xs:positiveInteger) as f:generator
{
  let $gen := if(not($gen?initialized)) then $gen =?> move-next()
                else $gen
   return
     if($gen?end-reached) then gn:empty-generator()
     else
       let $thisChunk := gn:to-array(gn:take($gen, $size)),
           $cutGen := gn:skip($gen, $size),
           $resultGen := $gen => map:put("get-current",   fn($this as f:generator){$thisChunk})
                              => map:put("move-next",   fn($this as f:generator){gn:chunk($cutGen, $size)})
        return $resultGen  
};

declare function gn:head($gen as f:generator) as item()* {gn:take($gen, 1) =?> get-current()};

declare function gn:tail($gen as f:generator) as f:generator {gn:skip-strict($gen, 1, true())};

declare function gn:at($gen as f:generator, $ind) as item()* {gn:subrange($gen, $ind, $ind) =?> get-current()};

declare function gn:contains($gen as f:generator, $value as item()*) as xs:boolean
     {
       let $gen := if(not($gen?initialized)) then  $gen =?> move-next()
                     else $gen
        return
          if($gen?end-reached) then false()
           else
             let $current := $gen =?> get-current()
               return
                  if(deep-equal($current, $value)) then true()
                   else gn:contains($gen =?> move-next(), $value) 
     };

declare function gn:for-each($gen as f:generator, $fun as function(*)) as f:generator
{
  let $gen := if(not($gen?initialized)) then $gen =?> move-next()
                else $gen        
   return
     if($gen?end-reached) then gn:empty-generator()
      else
       let $current := $fun($gen =?> get-current()),
            $newResultGen := map:put($gen, "get-current",   fn($this as f:generator){$current}),
            $nextGen := $gen =?> move-next()
        return
          if($nextGen?end-reached) then $newResultGen
            else
              let $newResultGen2 :=  map:put($newResultGen, "move-next",   fn($this as f:generator) {gn:for-each($nextGen, $fun)}) 
                 return
                   $newResultGen2         
};

declare function gn:for-each-pair($gen as f:generator, $gen2 as f:generator, $fun as function(*)) as f:generator
{
  let $gen := if(not($gen?initialized)) then $gen =?> move-next()
              else $gen,
      $gen2 := if(not($gen2?initialized)) then $gen2 =?> move-next()
              else $gen2
   return
      if($gen?end-reached or $gen2?end-reached) then gn:empty-generator() 
       else  
         let $current := $fun($gen =?> get-current(), $gen2 =?> get-current()),
             $newResultGen := map:put($gen, "get-current",   fn($this as f:generator){$current}),
             $nextGen1 := $gen =?> move-next(),
             $nextGen2 := $gen2 =?> move-next()
          return
             if($nextGen1?end-reached or $nextGen2?end-reached) then $newResultGen
               else
                 let $newResultGen2 := map:put($newResultGen, "move-next",   fn($this as f:generator){gn:for-each-pair($nextGen1, $nextGen2, $fun)})
                   return
                     $newResultGen2      
};

declare function gn:zip($gen as f:generator, $gen2 as f:generator) as f:generator
{
  gn:for-each-pair($gen, $gen2, fn($x1, $x2){[$x1, $x2]})
};

declare function gn:concat($gen as f:generator, $gen2 as f:generator) as f:generator
      {
        let $gen := if(not($gen?initialized)) then $gen =?> move-next()
                    else $gen,
            $gen2 := if(not($gen2?initialized)) then $gen2 =?> move-next()
                    else $gen2,
            $resultGen := if($gen?end-reached) then $gen2
                            else if($gen2?end-reached) then $gen
                            else
                              $gen  => map:put( "move-next", 
                                                  fn($this as f:generator)
                                                 {
                                                 let $nextGen := $gen =?> move-next()
                                                   return 
                                                     gn:concat($nextGen, $gen2)
                                                 }
                                              )                                   
        return 
           $resultGen            
      };
      
declare function gn:append($gen as f:generator, $value as item()*) as f:generator
      {
        let $gen := if(not($gen?initialized)) then $gen =?> move-next()
                    else $gen,
            $genSingle := $gen => map:put("get-current",   fn($this as f:generator){$value})
                               => map:put("move-next",   fn($this as f:generator){gn:empty-generator()})
                               => map:put("end-reached", false())
         return
           gn:concat($gen, $genSingle)                    
      };  
      
declare function gn:prepend($gen as f:generator, $value as item()*) as f:generator
      {
        let $gen := if(not($gen?initialized)) then $gen =?> move-next()
                    else $gen,
            $genSingle := gn:empty-generator() => gn:append($value)
         return
           gn:concat($genSingle, $gen)  
      };    
      
declare function gn:insert-at($gen as f:generator, $pos as xs:positiveInteger, $value as item()*) as f:generator
      {
        let $genTail := gn:skip-strict($gen, $pos - 1, true())
         return
            if($pos gt 1)
              then gn:concat(gn:append(gn:take($gen, $pos - 1), $value), $genTail)
              else gn:prepend($genTail, $value)               
      };     
      
declare function gn:remove-at($gen as f:generator, $pos as xs:nonNegativeInteger) as f:generator
      {
        let $genTail := gn:skip-strict($gen, $pos, true())
          return
            if($pos gt 1)
              then gn:concat(gn:take($gen, $pos - 1), $genTail)
              else $genTail
      };
      
declare function gn:remove-where($gen as f:generator, $predicate as function(item()*) as xs:boolean) as f:generator
      {
        let $gen := if(not($gen?initialized)) then $gen =?> move-next()
                      else $gen
          return
            gn:filter($gen, fn($x){not($predicate($x))})  
      };      

declare function gn:distinct($gen as f:generator) as f:generator
      {
        let $gen := if(not($gen?initialized)) then $gen =?> move-next()
                      else $gen
         return
           if($gen?end-reached) then $gen
           else
             let $priorValue := $gen =?> get-current()
               return
                 $gen => map:put("move-next",   fn($this as f:generator){gn:distinct(gn:remove-where(gn:tail($gen), fn($x){deep-equal($priorValue, $x)}))})  
      };
     
declare function gn:replace($gen as f:generator, $funIsMatching as function(item()*) as xs:boolean, $replacement as item()*) as f:generator
      {
        if($gen?end-reached) then $gen
          else
            let $current := $gen =?> get-current()
              return
                if($funIsMatching($current))
                  then let $nextGen := $gen =?> move-next()
                     return
                       $gen => map:put("get-current",   fn($this as f:generator) {$replacement})
                            => map:put("move-next",   fn($this as f:generator) { $nextGen } 
                                  )
                  else (: $current is not the match for replacement :)
                    let $nextGen := $gen =?> move-next()
                      return $gen => map:put("move-next", 
                                             fn($this as f:generator)
                                           {
                                             let $intendedReplace := function($z) {$z => gn:replace($funIsMatching, $replacement)}
                                              return
                                                if($nextGen?end-reached) then $nextGen
                                                else $intendedReplace($nextGen)
                                           }
                                        )
      };      
      
declare function gn:reverse($gen as f:generator) as f:generator
{
  if($gen?end-reached) then gn:empty-generator()
    else
     let $current := $gen =?> get-current()
       return
         gn:append(gn:reverse(gn:tail($gen)), $current)
};      
      
declare function gn:filter($gen as f:generator, $pred as function(item()*) as xs:boolean) as f:generator
{
 if($gen?initialized and $gen?end-reached) then gn:empty-generator()
  else
    let $getNextGoodGen := function($gen as map(*), 
                                 $pred as function(item()*) as xs:boolean)
       {
          if($gen?end-reached) then gn:empty-generator()
          else
            let $mapResult := 
                  while-do(
                           $gen,
                           function($x) { not($x?end-reached) and not($pred($x =?> get-current()))},
                           function($x) { $x =?> move-next() }
                           )   
            return 
              if($mapResult?end-reached) then gn:empty-generator()
               else $mapResult                  
       },
       
       $gen := if($gen?initialized) then $gen 
                 else $gen =?> move-next(),
       $nextGoodGen := $getNextGoodGen($gen, $pred)
    return
      if($nextGoodGen?end-reached) then gn:empty-generator()
      else
        $nextGoodGen  => map:put("inputGen", $nextGoodGen)
                     => map:put("move-next", 
                                  fn($this as f:generator) 
                                  {
                                    let $nextGoodGen := $getNextGoodGen($this?inputGen =?> move-next(), $pred)
                                      return
                                        if($nextGoodGen?end-reached) then gn:empty-generator()
                                        else
                                           $nextGoodGen => map:put("move-next",   
                                                                   fn($this as f:generator) {gn:filter($nextGoodGen =?> move-next(), $pred)}
                                                                   )
                                                        => map:put("inputGen", $nextGoodGen)
                                   }
                               )

  }; 
  
declare function gn:fold-left($gen as f:generator, $init as item()*, $action as fn(*)) as item()*
{
  if($gen?end-reached) then $init
    else gn:fold-left(gn:tail($gen), $action($init, $gen =?> get-current()), $action)
};

declare function gn:fold-right($gen as f:generator, $init as item()*, $action as fn(item()*, item()*) as item()*) as item()*
{
  if($gen?end-reached) then $init
    else $action(gn:head($gen), gn:fold-right(gn:tail($gen), $init, $action))
};

declare function gn:fold-lazy($gen as f:generator, $init as item()*, $action as fn(*), $shortCircuitProvider as function(*)) as item()*
{
  if($gen?end-reached) then $init
  else
   let $current := $gen =?> get-current()
     return
       if(function-arity($shortCircuitProvider($current, $init)) eq 0)
         then $shortCircuitProvider($current, $init)()
         else $action($current, gn:fold-lazy($gen =?> move-next(), $init, $action, $shortCircuitProvider))
};

declare function gn:scan-left($gen as f:generator, $init as item()*, $action as fn(*)) as f:generator
{
  let $resultGen := gn:empty-generator() 
                        => map:put("end-reached", false())
                        => map:put("get-current",   fn($this as f:generator){$init})
   return
     if($gen?end-reached) 
       then $resultGen => map:put("move-next",   fn($this as f:generator){gn:empty-generator()})
       else
         let $resultGen := $resultGen => map:put("get-current",   fn($this as f:generator){$init}),
             $partialFoldResult := $action($init, $gen =?> get-current())
           return
             let $nextGen := $gen =?> move-next()
              return
                $resultGen => map:put("move-next",   fn($this as f:generator)
                                      { 
                                          gn:scan-left($nextGen, $partialFoldResult, $action)
                                       }
                                      )            
};

declare function gn:scan-right($gen as f:generator, $init as item()*, $action as fn(*)) as f:generator
{
  gn:reverse(gn:scan-left(gn:reverse($gen), $init, $action))                         
};
 
declare function gn:make-generator($provider as function(array(*)) as array(*)) 
{
 let  $providerResult := $provider([]),
      $nextProviderState := $providerResult(1),
      $nextDataItemHolder := $providerResult(2)
    return
      let $nextGen := if(array:empty($nextDataItemHolder)) 
                   then gn:empty-generator()  
                   else gn:empty-generator()
                    => map:put("providerState", $nextProviderState)
                    => map:put("current", $nextDataItemHolder(1))
                    => map:put("end-reached", false())
                    => map:put("get-current", fn($this as f:generator) {$this?current})
                    => map:put("move-next",  
                                 fn($this as f:generator) 
                                {
                                  let $nextProviderResult := $provider($this?providerState),
                                      $nextDataItemHolder := $nextProviderResult(2)
                                    return
                                      if(array:empty($nextDataItemHolder)) then gn:empty-generator()
                                      else
                                        $this => map:put("current", $nextDataItemHolder(1))
                                          => map:put("providerState", $nextProviderResult(1))
                                }
                               )
                             
   return $nextGen                                            
};   

declare function gn:make-generator-from-array($input as array(*)) as f:generator
{
  let $size := array:size($input),
      $arrayProvider := fn($state as array(xs:integer?))
                        {
                          let $ind := if(array:empty($state))
                                      then 0
                                      else $state(1),
                              $newState := if($ind +1 gt $size) then []   
                                             else [$ind +1],
                              $newResult := if($ind +1 gt $size) then []
                                              else [$input($ind + 1)]
                           return [$newState, $newResult]
                        }
   return gn:make-generator($arrayProvider)
};  

declare function gn:make-generator-from-sequence($input as item()*) as f:generator
{
  let $size := count($input),
      $seqProvider := fn($state as array(xs:integer?))
                        {
                          let $ind := if(array:empty($state))
                                      then 0
                                      else $state(1),
                              $newState := if($ind +1 gt $size) then []   
                                             else [$ind +1],
                              $newResult := if($ind +1 gt $size) then []
                                              else [$input[$ind + 1]] 
                           return [$newState, $newResult]                  
                        }
   return gn:make-generator($seqProvider)
};   

declare function gn:make-generator-from-map($inputMap as map(*)) as f:generator
        {
          let $keys := map:keys($inputMap),
              $size := map:size($inputMap),
              $mapProvider := fn($state as array(xs:integer?))
              {
                
                let $ind := if(array:empty($state))
                              then 0
                              else $state(1),
                    $newState := if($ind +1 gt $size) then []   
                                   else [$ind +1],
                    $newResult := if($ind +1 gt $size) then []
                                    else
                                       let $key := $keys[$ind + 1]
                                        return
                                          [( $key, [ $inputMap($key) ])]
 
                  return [$newState, $newResult]                                      
             }                        
            return
              gn:make-generator($mapProvider)
        };

declare function gn:to-sequence($gen as f:generator) as item()* {gn:to-array($gen) => array:items()}; 

declare function gn:to-map($gen as f:generator) as map(*)
        {
          let $genEntries := $gen => gn:for-each(fn($x)
                           {
                             let $key := head($x),
                                 $tail := tail($x),
                                 $value := if($tail instance of array(*)) 
                                           then for $ind in 1 to array:size($tail)
                                                 return $tail($ind)
                                           else $tail
                             return
                               map:entry($key, $value)
                           }
                         ) 
           return
             map:merge($genEntries => gn:to-sequence())
        };    

declare function gn:empty-generator() as f:generator 
{
  f:generator(initialized := true(), end-reached := true(),
              get-current := fn($this as f:generator)
                                {error((),"get-current() called on an empty-generator")},
              move-next := fn($this as f:generator)
                                {error((),"move-next() called on an empty-generator")}
           )
};

declare record f:generator 
   ( initialized as xs:boolean,
     end-reached as xs:boolean,
     get-current as fn($this as f:generator) as item()*,
     move-next as   fn($this as f:generator) as f:generator,           
     *
   );


let $gen2ToInf := f:generator(initialized := true(), end-reached := false(), 
                              get-current :=   fn($this as f:generator){$this?last +1},
                              move-next :=   fn($this as f:generator)
                              {
                                if(not($this?initialized))
                                  then map:put($this, "initialized", true())
                                  else map:put($this, "last", $this?last + 1)
                              }
                              (: , options := {"last" : 1} :)
                             ) => map:put("last", 1),
    $genEmpty := f:generator(initialized := true(), end-reached := false(),
                             get-current := fn($this as f:generator)
                                              {error((),"get-current() called on an empty-generator")},
                             move-next := fn($this as f:generator)
                                              {error((),"move-next() called on an empty-generator")}      
                            ),
    $genN := $gen2ToInf => gn:for-each(fn($n) {$n - 1}),
    $gen0toInf := $gen2ToInf => gn:for-each(fn($n) {$n - 2}),
    $double := fn($n) {2*$n},
    $sum2 := fn($m, $n) {$m + $n},
    $product := fn($m, $n) {$m * $n},
    $factorial := fn($n) {fold-left(1 to $n, 1, $product)}
  return    
  (
    "$gen2ToInf => gn:take(3) => gn:to-array()",
    $gen2ToInf => gn:take(3) => gn:to-array(),
    "$gen2ToInf => gn:take(10000000) => gn:value()",
    $gen2ToInf => gn:take(100000000) => gn:value(),
    "================",    
    "$gen2ToInf => gn:take(3) => gn:skip(2) => gn:value()",
    $gen2ToInf => gn:take(3) => gn:skip(2) => gn:value(),
    (: $gen2ToInf => gn:take(3) =?> move-next() =?> move-next() =?> move-next() =?> get-current(), :)
    "================",
    "$gen2ToInf => gn:value()",
    $gen2ToInf => gn:value(),
    "$gen2ToInf => gn:next() => gn:value()",
    $gen2ToInf => gn:next()  => gn:value(),
    "================",
    "$gen2ToInf => gn:take(5) instance of f:generator",
    $gen2ToInf => gn:take(5) instance of f:generator,
    gn:empty-generator() => gn:take(5) => gn:to-array(),
    "==>  $gen2ToInf => gn:skip(7) instance of f:generator",
    $gen2ToInf => gn:skip(7) instance of f:generator,  
    "================",
    "$gen2ToInf => gn:subrange(4, 6) => gn:value()",
    $gen2ToInf => gn:subrange(4, 6) => gn:value(), 
    "$gen2ToInf => gn:subrange(4, 6) => gn:next() => gn:value()",
    $gen2ToInf => gn:subrange(4, 6) => gn:next() => gn:value(),
    "$gen2ToInf => gn:subrange(4, 6) => gn:next() => gn:next() => gn:value()",
    $gen2ToInf => gn:subrange(4, 6) => gn:next() => gn:next() => gn:value(),
    (: $gen2ToInf => gn:subrange(4, 6) =?> move-next() =?> move-next() =?> move-next() =?> get-current() :) (: Must raise error:)    
    "================",    
    "$gen2ToInf => gn:subrange(4, 6) => gn:head()",
    $gen2ToInf => gn:subrange(4, 6) => gn:head(),  
    "$gen2ToInf => gn:subrange(4, 6) => gn:tail() => gn:head()",
    $gen2ToInf => gn:subrange(4, 6) => gn:tail() => gn:head(),
    "$gen2ToInf => gn:subrange(4, 6) => gn:to-array()",
    $gen2ToInf => gn:subrange(4, 6) => gn:to-array(),
    "$gen2ToInf => gn:head()",
    $gen2ToInf => gn:head(),
    "==>  $gen2ToInf => gn:tail() => gn:head()",
    $gen2ToInf => gn:tail() => gn:head(),
    "================", 
    "$gen2ToInf => gn:subrange(4, 6) => gn:tail() => gn:to-array()",
    $gen2ToInf => gn:subrange(4, 6) => gn:tail() => gn:to-array(),
    (: $gen2ToInf => gn:empty-generator() => gn:tail() => gn:to-array(), :)
    "================",
    "$gen2ToInf => gn:at(5)",
    $gen2ToInf => gn:at(5), 
    "================",
    "$gen2ToInf => gn:subrange(1, 5) => gn:to-array()",
    $gen2ToInf => gn:subrange(1, 5) => gn:to-array(),
    "$gen2ToInf => gn:subrange(1, 5) => gn:for-each($double) => gn:to-array()",
    $gen2ToInf => gn:subrange(1, 5) => gn:for-each($double) => gn:to-array(),
    "$gen2ToInf => gn:take(5) => gn:for-each($double) => gn:to-array()",
    $gen2ToInf => gn:take(5) => gn:for-each($double) => gn:to-array(),
    "==>  $gen2ToInf => gn:for-each($double) => gn:take(5) => gn:to-array()",
    $gen2ToInf => gn:for-each($double) => gn:take(5) => gn:to-array(),
    gn:empty-generator() => gn:for-each($double) => gn:to-array(),
    "================",
    "$gen2ToInf => gn:subrange(1, 5) => gn:to-array()",
    $gen2ToInf => gn:subrange(1, 5) => gn:to-array(),
    "$gen2ToInf => gn:subrange(6, 10) => gn:to-array()",
    $gen2ToInf => gn:subrange(6, 10) => gn:to-array(),
    "$gen2ToInf => gn:subrange(1, 5) => gn:for-each-pair($gen2ToInf => gn:subrange(6, 10), $sum2) => gn:to-array()",
    $gen2ToInf => gn:subrange(1, 5) => gn:for-each-pair($gen2ToInf => gn:subrange(6, 10), $sum2) => gn:to-array(), 
    "==>  $gen2ToInf => gn:for-each-pair($gen2ToInf, $sum2) => gn:take(5) => gn:to-array()",
    $gen2ToInf => gn:for-each-pair($gen2ToInf, $sum2) => gn:take(5) => gn:to-array(),
    "================",
    "==>  $gen2ToInf => gn:filter(fn($n){$n mod 2 eq 1}) => gn:value()",
    $gen2ToInf => gn:filter(fn($n){$n mod 2 eq 1}) => gn:value(),
    "$gen2ToInf => gn:filter(fn($n){$n mod 2 eq 1}) => gn:next() => gn:value()",
    $gen2ToInf => gn:filter(fn($n){$n mod 2 eq 1}) => gn:next() => gn:value(),
    "$gen2ToInf => gn:take(10) => gn:filter(fn($n){$n gt 12}) => gn:to-array()",
    $gen2ToInf => gn:take(10) => gn:filter(fn($n){$n gt 12}) => gn:to-array(),
    "gn:empty-generator() => gn:filter(fn($n){$n eq $n}) => gn:to-array()",
    gn:empty-generator() => gn:filter(fn($n){$n eq $n}) => gn:to-array(),
    "================", 
    "$gen2ToInf => gn:filter(fn($n){$n mod 2 eq 1}) => gn:take(10) => gn:to-array()",
    $gen2ToInf => gn:filter(fn($n){$n mod 2 eq 1}) => gn:take(10) => gn:to-array(),  
    "================", 
    "$gen2ToInf => gn:filter(fn($n){$n mod 2 eq 1}) => gn:take(10) => gn:to-sequence()",
    $gen2ToInf => gn:filter(fn($n){$n mod 2 eq 1}) => gn:take(10) => gn:to-sequence(),
    "================", 
    "$gen2ToInf => gn:take-while(fn($n){$n lt 11}) => gn:to-array()",
    $gen2ToInf => gn:take-while(fn($n){$n lt 11}) => gn:to-array(), 
    "$gen2ToInf => gn:take-while(fn($n){$n lt 2}) => gn:to-array()",
    $gen2ToInf => gn:take-while(fn($n){$n lt 2}) => gn:to-array(), 
    "$gen2ToInf => gn:take-while(fn($n){$n lt 100000000}) => gn:value()",
    $gen2ToInf => gn:take-while(fn($n){$n lt 100000000}) => gn:value(), 
    "================", 
    "$gen2ToInf => gn:skip-while(fn($n){$n lt 11}) => gn:take(5) => gn:to-array()",
    $gen2ToInf => gn:skip-while(fn($n){$n lt 11}) => gn:take(5) => gn:to-array(),
    "==> $gen2ToInf => gn:skip-while(fn($n){$n lt 2}) => gn:take(5) => gn:to-array()",
    $gen2ToInf => gn:skip-while(fn($n){$n lt 2})=> gn:take(5) => gn:to-array(),
    "gn:empty-generator() => gn:skip-while(fn($n){$n lt 100}) => gn:to-array()",
    gn:empty-generator() => gn:skip-while(fn($n){$n lt 100}) => gn:to-array(),
    "
     ==> $gen2ToInf => gn:skip-while(fn($n){$n lt 2}) => gn:skip(1)",
     $gen2ToInf => gn:skip-while(fn($n){$n lt 2}) => gn:skip(1),
    "$gen2ToInf => gn:some()",
     $gen2ToInf => gn:some(),
     "gn:empty-generator() => gn:some()",
     gn:empty-generator() => gn:some(),
    "================",
    "$gen2ToInf => gn:take(5) => gn:filter(fn($n){$n ge 7}) => gn:some()",
     $gen2ToInf => gn:take(5) => gn:filter(fn($n){$n ge 7}) => gn:some(),  
     "$gen2ToInf => gn:take(5) => gn:some-where(fn($n){$n ge 7})",
     $gen2ToInf => gn:take(5) => gn:some-where(fn($n){$n ge 7}), 
     "$gen2ToInf => gn:take(5) => gn:some-where(fn($n){$n ge 6})",
     $gen2ToInf => gn:take(5) => gn:some-where(fn($n){$n ge 6}),
     "$gen2ToInf => gn:some-where(fn($n){$n ge 100})",
     $gen2ToInf => gn:some-where(fn($n){$n ge 100}),
     "================",
     "$gen2ToInf => gn:take(10) => gn:take(11) => gn:to-array()",
     $gen2ToInf => gn:take(10) => gn:take(11) => gn:to-array(),
     "$gen2ToInf => gn:take(10) => gn:skip(10) => gn:to-array()",
     $gen2ToInf => gn:take(10) => gn:skip(10) => gn:to-array(),
     "$gen2ToInf => gn:take(10) => gn:skip(9) => gn:to-array()",     
     $gen2ToInf => gn:take(10) => gn:skip(9) => gn:to-array(),
     "$gen2ToInf => gn:take(10) => gn:subrange(3, 13) => gn:to-array()",
     $gen2ToInf => gn:take(10) => gn:subrange(3, 13) => gn:to-array(),
     "$gen2ToInf => gn:take(10) => gn:subrange(5, 3) => gn:to-array()",
     $gen2ToInf => gn:take(10) => gn:subrange(5, 3) => gn:to-array(),
     "================",
     "$gen2ToInf => gn:chunk(10) => gn:value()",
      $gen2ToInf => gn:chunk(10) => gn:value(),
      "gn:empty-generator() => gn:chunk(20) => gn:some()",
      gn:empty-generator() => gn:chunk(20) => gn:some(),
      "==>  $gen2ToInf => gn:chunk(20) => gn:take(5) => gn:to-array()",
      $gen2ToInf => gn:chunk(20) => gn:take(5) => gn:to-array(),
     "================",
     "$gen2ToInf => gn:chunk(10) => gn:next() => gn:value()",
      $gen2ToInf => gn:chunk(10) => gn:next() => gn:value(),
     "$gen2ToInf => gn:take(100) => gn:chunk(20) => gn:next() => gn:next() => gn:value()", 
      $gen2ToInf => gn:take(100) => gn:chunk(20) => gn:next() => gn:next() => gn:value(),
     "$gen2ToInf => gn:take(100) => gn:chunk(20) => gn:skip(1) => gn:value()",      
      $gen2ToInf => gn:take(100) => gn:chunk(20) => gn:skip(1) => gn:value(),
     "================",      
     "$gen2ToInf => gn:take(100) => gn:chunk(20) => gn:for-each(fn($genX){$genX}) => gn:to-array()",      
      $gen2ToInf => gn:take(100) => gn:chunk(20) => gn:for-each(fn($genX){$genX}) => gn:to-array(),
     "================",  
     "$gen2ToInf => gn:take(10) => gn:chunk(4) => gn:to-array()",
      $gen2ToInf => gn:take(10) => gn:chunk(4) => gn:to-array(),
      "$gen2ToInf => gn:take(10) => gn:chunk(4) => gn:for-each(fn($arr){array:size($arr)}) => gn:to-array()",
      $gen2ToInf => gn:take(10) => gn:chunk(4) => gn:for-each(fn($arr){array:size($arr)}) => gn:to-array(),
     "================", 
     "$gen2ToInf => gn:subrange(10, 15) => gn:concat($gen2ToInf => gn:subrange(1, 9)) => gn:to-array()",
     $gen2ToInf => gn:subrange(10, 15) => gn:concat($gen2ToInf => gn:subrange(1, 9)) => gn:to-array(),
     "gn:empty-generator() => gn:concat(gn:empty-generator()) => gn:to-array()",
     gn:empty-generator() => gn:concat(gn:empty-generator()) => gn:to-array(),
     "gn:empty-generator() => gn:concat($gen2ToInf => gn:take(1)) => gn:to-array()",
     gn:empty-generator() => gn:concat($gen2ToInf => gn:take(1)) => gn:to-array(),
     "$gen2ToInf => gn:take(1) => gn:concat(gn:empty-generator()) => gn:to-array()",
     $gen2ToInf => gn:take(1) => gn:concat(gn:empty-generator()) => gn:to-array(),
     "$gen2ToInf => gn:concat($gen2ToInf) => gn:value()",
     $gen2ToInf => gn:concat($gen2ToInf) => gn:value(),
     "================", 
     "$gen2ToInf => gn:subrange(1, 5) => gn:append(101) => gn:to-array()",
     $gen2ToInf => gn:subrange(1, 5) => gn:append(101) => gn:to-array(),
     "$gen2ToInf => gn:subrange(1, 5) => gn:prepend(101) => gn:to-array()",
     $gen2ToInf => gn:subrange(1, 5) => gn:prepend(101) => gn:to-array(),
     "==>  $gen2ToInf => gn:append(101)",
     $gen2ToInf => gn:append(101),
     "==>  $gen2ToInf => gn:append(101) => gn:value()",
     $gen2ToInf => gn:append(101) => gn:value(),
     "$gen2ToInf => gn:append(101) instance of f:generator",
     $gen2ToInf => gn:append(101) instance of f:generator,
     "$gen2ToInf => gn:take(5) => gn:append(101) => gn:to-array()",
     $gen2ToInf => gn:take(5) => gn:append(101) => gn:to-array(),
     "gn:empty-generator() => gn:append(101) => gn:to-array()",
     gn:empty-generator() => gn:append(101) => gn:to-array(),
     "$gen2ToInf => gn:prepend(101) => gn:take(5) => gn:to-array()",
     $gen2ToInf => gn:prepend(101) => gn:take(5) => gn:to-array(),
     "================", 
     "$gen2ToInf => gn:subrange(1, 5) => gn:zip($gen2ToInf => gn:subrange(6, 10)) => gn:to-array()",
     $gen2ToInf => gn:subrange(1, 5) => gn:zip($gen2ToInf => gn:subrange(6, 10)) => gn:to-array(),
     "$gen2ToInf => gn:subrange(1, 5) => gn:zip($gen2ToInf => gn:subrange(10, 20)) => gn:to-array()",
     $gen2ToInf => gn:subrange(1, 5) => gn:zip($gen2ToInf => gn:subrange(10, 20)) => gn:to-array(),
     "==>  $gen2ToInf => gn:zip($gen2ToInf => gn:skip(5)) => gn:take(10) => gn:to-array()",
     $gen2ToInf => gn:zip($gen2ToInf => gn:skip(5)) => gn:take(10) => gn:to-array(),
     "$gen2ToInf => gn:subrange(1, 5) => gn:zip($gen2ToInf => gn:subrange(10, 20)) => gn:zip($gen2ToInf => gn:subrange(30, 40)) => gn:to-array()",
     $gen2ToInf => gn:subrange(1, 5) => gn:zip($gen2ToInf => gn:subrange(10, 20)) => gn:zip($gen2ToInf => gn:subrange(30, 40)) => gn:to-array(),
     "================", 
     "gn:make-generator(fn($state as array(*))
                        {
                          let $numGenerated := if(array:empty($state)) then 0
                                               else $state[1]
                            return
                               if($numGenerated le 9) then [ [$numGenerated + 1], [$numGenerated + 1] ]
                                 else [[-1], []]
                        } 
                       ) => gn:to-array(),",

     gn:make-generator(fn($state as array(*))
                                 {
                                   let $numGenerated := if(array:empty($state)) then 0
                                                        else $state[1]
                                     return
                                        if($numGenerated le 9) then [ [$numGenerated + 1], [$numGenerated + 1] ]
                                          else [[-1], []]
                                 } 
                             ) => gn:to-array(),
     "gn:make-generator(fn($state as array(*))
                          {
                            let $numGenerated := if(array:empty($state)) then 0
                                                 else $state[1]
                             return
                               [ [$numGenerated + 1], [$numGenerated + 1] ]
                          } 
                        ) => gn:value()",                             
     gn:make-generator(fn($state as array(*))
                                 {
                                   let $numGenerated := if(array:empty($state)) then 0
                                                        else $state[1]
                                     return
                                        [ [$numGenerated + 1], [$numGenerated + 1] ]
                                 } 
                             ) => gn:value(),  
     "gn:make-generator(fn($state as array(*))
                          {
                            let $numGenerated := if(array:empty($state)) then 0
                                                 else $state[1]
                              return
                                 [ [$numGenerated + 1], [$numGenerated + 1] ]
                          } 
                       ) => gn:subrange(10, 15) => gn:to-array()",                                                        
     gn:make-generator(fn($state as array(*))
                                 {
                                   let $numGenerated := if(array:empty($state)) then 0
                                                        else $state[1]
                                     return
                                        [ [$numGenerated + 1], [$numGenerated + 1] ]
                                 } 
                             ) => gn:subrange(10, 15) => gn:to-array(),                             
     "================", 
     "gn:make-generator-from-array([1, 4, 9, 16, 25]) => gn:to-array()",
      gn:make-generator-from-array([1, 4, 9, 16, 25]) => gn:to-array(),
      gn:make-generator-from-array([]) => gn:to-array(),      
      "gn:make-generator-from-sequence((1, 8, 27, 64, 125)) => gn:to-array()",
      gn:make-generator-from-sequence((1, 8, 27, 64, 125)) => gn:to-array(), 
     "================", 
     "$gen2ToInf => gn:take(10) => gn:insert-at(3, ""XYZ"") => gn:to-array()",
      $gen2ToInf => gn:take(10) => gn:insert-at(3, "XYZ") => gn:to-array(),
      "$gen2ToInf => gn:take(10) => gn:insert-at(1, ""ABC"") => gn:to-array()",
      $gen2ToInf => gn:take(10) => gn:insert-at(1, "ABC") => gn:to-array(),
      "$gen2ToInf => gn:take(10) => gn:insert-at(11, ""PQR"") => gn:to-array()",
      $gen2ToInf => gn:take(10) => gn:insert-at(11, "PQR") => gn:to-array(),
      "==>  $gen2ToInf => gn:insert-at(3, ""XYZ"") => gn:take(10) => gn:to-array()", 
      $gen2ToInf => gn:insert-at(3, "XYZ") => gn:take(10) => gn:to-array(),
     (:  $gen2ToInf => gn:take(10) => gn:insert-at(12, "GHI") => gn:to-array(), :)  (:  Must raise error "Input Generator too-short." :) 
     "================", 
     "$gen2ToInf => gn:take(10) => gn:remove-at(3) => gn:to-array()",
      $gen2ToInf => gn:take(10) => gn:remove-at(3) => gn:to-array(),
      "$gen2ToInf => gn:take(10) => gn:remove-at(1) => gn:to-array()",
      $gen2ToInf => gn:take(10) => gn:remove-at(1) => gn:to-array(),
      "$gen2ToInf => gn:take(10) => gn:remove-at(10) => gn:to-array()",
      $gen2ToInf => gn:take(10) => gn:remove-at(10) => gn:to-array(),
      "==>  $gen2ToInf => gn:remove-at(3) => gn:take(10) => gn:to-array()",
      $gen2ToInf => gn:remove-at(3) => gn:take(10) => gn:to-array(),
      (: , $gen2ToInf => gn:take(10) => gn:remove-at(11) => gn:to-array() :)        (:  Must raise error "Input Generator too-short." :) 
(::) 
     "================",
     "==>  $gen2ToInf => gn:remove-where(fn($x){$x mod 3 eq 0}) => gn:take(10) => gn:to-array()",
      $gen2ToInf => gn:remove-where(fn($x){$x mod 3 eq 0}) => gn:take(10) => gn:to-array(),   
       
     "================",
     "gn:make-generator-from-sequence((1,  3, 1, 2,  1, 2, 5, 2, 5)) => gn:distinct() => gn:to-array()",
      gn:make-generator-from-sequence((1,  3, 1, 2,  1, 2, 5, 2, 5)) => gn:distinct() => gn:to-array(),
      "$gen2ToInf => gn:for-each(fn($n){$n idiv 10}) => gn:take(50) => gn:distinct() => gn:to-array()",
      $gen2ToInf => gn:for-each(fn($n){$n idiv 10}) => gn:take(50) => gn:distinct() => gn:to-array(),
      "$gen2ToInf => gn:for-each(fn($n){$n idiv 10}) => gn:take(100) => gn:distinct() => gn:to-array()",
      $gen2ToInf => gn:for-each(fn($n){$n idiv 10}) => gn:take(100) => gn:distinct() => gn:to-array(),
      "==> $gen2ToInf => gn:for-each(fn($n){$n idiv 10}) => gn:distinct() => gn:take(35) => gn:to-array()",
      $gen2ToInf => gn:for-each(fn($n){$n idiv 10}) => gn:distinct() => gn:take(35) => gn:to-array(),
      "gn:empty-generator() => gn:distinct() => gn:to-array()",
      gn:empty-generator() => gn:distinct() => gn:to-array(),
     "================",          
     "$gen2ToInf => gn:take(10) => gn:replace(fn($x){$x gt 4}, ""Replacement"") => gn:to-array()",
      $gen2ToInf => gn:take(10) => gn:replace(fn($x){$x gt 4}, "Replacement") => gn:to-array(),
      "$gen2ToInf => gn:take(10) => gn:replace(fn($x){$x lt 3}, ""Replacement"") => gn:to-array()",
      $gen2ToInf => gn:take(10) => gn:replace(fn($x){$x lt 3}, "Replacement") => gn:to-array(),
      "$gen2ToInf => gn:take(10) => gn:replace(fn($x){$x gt 10}, ""Replacement"") => gn:to-array()",
      $gen2ToInf => gn:take(10) => gn:replace(fn($x){$x gt 10}, "Replacement") => gn:to-array(),
      "$gen2ToInf => gn:take(10) => gn:replace(fn($x){$x gt 11}, ""Replacement"") => gn:to-array()",
      $gen2ToInf => gn:take(10) => gn:replace(fn($x){$x gt 11}, "Replacement") => gn:to-array(),
      "$gen2ToInf => gn:take(10) => gn:replace(fn($x){$x lt 2}, ""Replacement"") => gn:to-array()",
      $gen2ToInf => gn:take(10) => gn:replace(fn($x){$x lt 2}, "Replacement") => gn:to-array(),
      "==> $gen2ToInf => gn:replace(fn($x){$x gt 4}, ""Replacement"") => gn:take(10) => gn:to-array()",
      $gen2ToInf => gn:replace(fn($x){$x gt 4}, "Replacement") => gn:take(10) => gn:to-array(),
      "$gen2ToInf => gn:replace(fn($x){$x lt 3}, ""Replacement"") => gn:take(10) => gn:to-array()",
      $gen2ToInf => gn:replace(fn($x){$x lt 3}, "Replacement") => gn:take(10) => gn:to-array(),
    (:  
      Will result in endless loop:
      
      , "==>  ==>  ==>  $gen2ToInf => gn:replace(fn($x){$x lt 2}, ""Replacement"") => gn:take(10) => gn:to-array() <==  <==  <==",
      $gen2ToInf?replace2(fn($x){$x lt 2}, "Replacement") => gn:take(10) => gn:to-array() 
    :)
    "================",
    "gn:empty-generator() => gn:reverse() => gn:to-array()",
    gn:empty-generator() => gn:reverse() => gn:to-array(),
    "gn:empty-generator() => gn:append(2) => gn:reverse() => gn:to-array()",
    gn:empty-generator() => gn:append(2) => gn:reverse() => gn:to-array(),
    "$gen2ToInf => gn:take(10) => gn:reverse() => gn:to-array()",
    $gen2ToInf => gn:take(10) => gn:reverse() => gn:to-array(),
    "================",
    "$genN => gn:take(10) => gn:contains(3)",
    $genN => gn:take(10) => gn:contains(3),
    "$genN => gn:take(10) => gn:contains(20)",
    $genN => gn:take(10) => gn:contains(20),
    "$genN => gn:take(10) => gn:contains(1)",    
    $genN => gn:take(10) => gn:contains(1), 
    "$genN => gn:take(10) => gn:contains(10)",     
    $genN => gn:take(10) => gn:contains(10),  
    "$genN => gn:take(10) => gn:contains(0)",
    $genN => gn:take(10) => gn:contains(0), 
    "$genN => gn:take(10) => gn:contains(11)",        
    $genN => gn:take(10) => gn:contains(11),
    "==> $genN => gn:contains(15)",    
    $genN => gn:contains(15), 
    "================",
    "$gen2ToInf => gn:first-where(fn($n){$n gt 10})",
    $gen2ToInf => gn:first-where(fn($n){$n gt 10}),
    "$gen2ToInf => gn:chunk(10) =>  gn:first-where(fn($arr as array(*)){$arr(1) le 33 and $arr(10) ge 33})",
    $gen2ToInf => gn:chunk(10) =>  gn:first-where(fn($arr as array(*)){$arr(1) le 33 and $arr(10) ge 33}), 
    "================",
    "$gen2ToInf => gn:take(5) => gn:fold-left(0, fn($x, $y){$x + $y})",
    $gen2ToInf => gn:take(5) => gn:fold-left(0, fn($x, $y){$x + $y}),
    "gn:empty-generator() => gn:fold-left(12345, fn($x, $y){$x + $y})",
    gn:empty-generator() => gn:fold-left(54321, fn($x, $y){$x + $y}),
    "================",
    "$gen2ToInf => gn:take(5) => gn:fold-right(0, fn($x, $y){$x + $y})",
    $gen2ToInf => gn:take(5) => gn:fold-right(0, fn($x, $y){$x + $y}),
    "gn:empty-generator() => gn:fold-right(12345, fn($x, $y){$x + $y})",
    gn:empty-generator() => gn:fold-right(12345, fn($x, $y){$x + $y}),
    "================",
    "==> $gen0toInf => gn:for-each(fn($n){(2 * $n + 1) div $factorial(2*xs:decimal($n)})
              => gn:take(8) => gn:fold-left(0, fn($x, $y){$x + $y})",
    $gen0toInf => gn:for-each(fn($n){(2*$n + 1) div $factorial(2*xs:decimal($n))}) => gn:take(8) => gn:fold-left(0, fn($x, $y){$x + $y}),
    "================",    
    "$gen0toInf => gn:for-each(fn($n){(2*$n + 1) div $factorial(2*xs:decimal($n))}) => gn:take(8) => gn:scan-left(0, fn($x, $y){$x + $y}) => gn:to-array()",
    $gen0toInf => gn:for-each(fn($n){(2*$n + 1) div $factorial(2*xs:decimal($n))}) => gn:take(8) => gn:scan-left(0, fn($x, $y){$x + $y}) => gn:to-array(),
    "================",
    "let $genSeqE := $gen0toInf => gn:for-each(fn($n){(2*$n + 1) div $factorial(2*xs:decimal($n))}) => gn:take(8) => gn:scan-left(0, fn($x, $y){$x + $y}),
    $genSeqE-Next := $genSeqE => gn:tail(),
    $genZipped := $genSeqE => gn:zip($genSeqE-Next)
 return
    $genZipped => gn:first-where(fn($pair){abs($pair(1) - $pair(2)) lt 0.000001})(2)",
    let $genSeqE := ($gen0toInf => gn:for-each(fn($n){(2*$n + 1) div $factorial(2*xs:decimal($n))}) => gn:take(8)) => gn:scan-left(0, fn($x, $y){$x + $y}),
        $genSeqE-Next := $genSeqE => gn:tail(),
        $genZipped := $genSeqE => gn:zip($genSeqE-Next)
      return
        ($genZipped => gn:first-where(fn($pair){abs($pair(1) - $pair(2)) lt 0.000001}))(2),        
    "================",
    
    "gn:empty-generator() => gn:scan-left(0, fn($x, $y){$x + $y}) => gn:to-array()",
    gn:empty-generator() => gn:scan-left(0, fn($x, $y){$x + $y}) => gn:to-array(),
    "$gen2ToInf => gn:take(5) => gn:scan-left(0, fn($x, $y){$x + $y}) => gn:to-array()",
    $gen2ToInf => gn:take(5) => gn:scan-left(0, fn($x, $y){$x + $y}) => gn:to-array(),
    "================",
    "gn:make-generator-from-sequence((1 to 10)) => gn:scan-right(0, fn($x, $y){$x + $y}) => gn:to-array()",
    gn:make-generator-from-sequence((1 to 10)) => gn:scan-right(0, fn($x, $y){$x + $y}) => gn:to-array(),
    $genN => gn:take(10) => gn:scan-right(0, fn($x, $y){$x + $y}) => gn:to-array(),
    "================",
    let $multShortCircuitProvider := fn($x, $y)
        {
          if($x eq 0) then fn(){0}
            else fn($z) {$x * $z}
        },
        $gen-5ToInf := $gen2ToInf => gn:for-each(fn($n){$n -7})
     return
     (
       "let $multShortCircuitProvider := fn($x, $y)
        {
          if($x eq 0) then fn(){0}
            else fn($z) {$x * $z}
        },
            $gen-5ToInf := $gen2ToInf => gn:for-each(fn($n){$n -7})
          return
            $gen2ToInf => gn:take(5) => gn:fold-lazy(1, $product, $multShortCircuitProvider),
            $gen-5ToInf => gn:fold-lazy(1, $product, $multShortCircuitProvider)",
       $gen2ToInf => gn:take(5) => gn:fold-lazy(1, $product, $multShortCircuitProvider),
       $gen-5ToInf => gn:fold-lazy(1, $product, $multShortCircuitProvider)
     ),
     "===============",
     "     let $myMap := {'John': 22, 'Ann': 28, 'Peter': 31}
      return 
        gn:make-generator-from-map($myMap) => gn:to-array()",
     let $myMap := {"John": 22, "Ann": 28, "Peter": 31}
      return 
        gn:make-generator-from-map($myMap) => gn:to-array(),
     "===============",        
     "let $myMap := {'John': 22, 'Ann': 28, 'Peter': 31},
          $genMap := gn:make-generator-from-map($myMap)
      return
        $genMap => gn:to-map()" ,
     let $myMap := {"John": 22, "Ann": 28, "Peter": 31},
         $genMap := gn:make-generator-from-map($myMap)
      return
        $genMap => gn:to-map(),
     "$gen2ToInf => gn:take(10) => gn:to-map()",
     $gen2ToInf => gn:take(10) => gn:to-map()
     (:  , gn:empty-generator() => gn:to-map() :)
     (: , $gen2ToInf => gn:make-generator-from-array([(), 5])  => gn:to-map()    :)  
   )
