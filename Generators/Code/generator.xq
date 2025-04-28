declare namespace f = "http://www.w3.org/2005/xpath-functions-2025";
declare namespace gn = "http://www.w3.org/2005/xpath-functions-2025/generator";
declare function gn:toArray($gen as f:generator)
{
   while-do( [$gen, []],
          function( $inArr) 
          { $inArr(1)?initialized and not($inArr(1)?endReached) },                 
          function($inArr) 
          { array{$inArr(1)?moveNext(), 
                  array:append($inArr(2), $inArr(1)?getCurrent())
                 } 
           }         
 ) (2)
};

declare function gn:take($gen as f:generator, $n as xs:integer)
{
  let $gen := if(not($gen?initialized)) then $gen?moveNext()
                else $gen
   return
     if($gen?endReached or $n le 0) then gn:emptyGenerator($gen)
      else
        let $current := $gen?getCurrent(),
            $newResultGen := map:put($gen, "getCurrent", %method fn(){$current}),
            $nextGen := $gen?moveNext()
         return
           if($nextGen?endReached) then $newResultGen
             else
               let
                   $newResultGen2 :=  map:put($newResultGen, "moveNext", %method fn() {gn:take($nextGen, $n -1)}) 
                 return
                   $newResultGen2  
};

declare function gn:takeWhile($gen as f:generator, $pred as function(item()*) as xs:boolean)
{
  let $gen := if(not($gen?initialized)) then $gen?moveNext()
                else $gen
   return
     if($gen?endReached) then gn:emptyGenerator($gen)
      else      
        let $current := $gen?getCurrent()
          return
            if(not($pred($current))) then gn:emptyGenerator($gen)
            else
              let $newResultGen := map:put($gen, "getCurrent", %method fn(){$current}),
                  $nextGen := $gen?moveNext()
               return
                  if($nextGen?endReached) then $newResultGen
                  else
                    let $newResultGen2 :=  map:put($newResultGen, "moveNext", %method fn() {gn:takeWhile($nextGen, $pred)}) 
                     return $newResultGen2    
};

declare function gn:skipStrict($gen as f:generator, $n as xs:nonNegativeInteger, $issueErrorOnEmpty as xs:boolean)
{
  if($n eq 0) then $gen
    else if($gen?endReached) 
           then if($issueErrorOnEmpty)
                 then error((), "Input Generator too-short") 
                 else gn:emptyGenerator($gen)
    else 
      let $gen := if(not($gen?initialized)) then $gen?moveNext()
                   else $gen
        return
          if(not($gen?endReached)) then gn:skipStrict($gen?moveNext(), $n -1, $issueErrorOnEmpty)
            else gn:emptyGenerator($gen)    
};

declare function gn:skip($gen as f:generator, $n as xs:nonNegativeInteger)
{
  gn:skipStrict($gen, $n, false())
};

declare function gn:skipWhile($gen as f:generator, $pred as function(item()*) as xs:boolean)
{
  let $gen := if(not($gen?initialized)) then $gen?moveNext()
                else $gen
   return
     if($gen?endReached) then gn:emptyGenerator($gen)
      else
        let $current := $gen?getCurrent()
         return
           if(not($pred($current))) then $gen
            else gn:skipWhile($gen?moveNext(), $pred)  
};

declare function gn:subrange($gen as f:generator, $m as xs:positiveInteger, $n as xs:positiveInteger)
{
 gn:take(gn:skip($gen, $m - 1), $n - $m + 1)  
};

declare function gn:some($gen as f:generator)
{
 $gen?initialized and not($gen?endReached)  
};

declare function gn:someWhere($gen as f:generator, $pred)
{
 gn:some(gn:filter($gen, $pred))
};

declare function gn:chunk($gen as f:generator, $size as xs:positiveInteger)
{
  let $gen := if(not($gen?initialized)) then $gen?moveNext()
                else $gen
   return
     if($gen?endReached) then gn:emptyGenerator($gen)
     else
       let $thisChunk := gn:toArray(gn:take($gen, $size)),
           $cutGen := gn:skip($gen, $size),
           $resultGen := $gen => map:put("getCurrent", %method fn(){$thisChunk})
                              => map:put("moveNext", %method fn(){gn:chunk($cutGen, $size)})
        return $resultGen  
};

declare function gn:head($gen as f:generator) {gn:take($gen, 1)?getCurrent()};

declare function gn:tail($gen as f:generator) {gn:skip($gen, 1)};

declare function gn:at($gen as f:generator, $ind) {gn:subrange($gen, $ind, $ind)?getCurrent()};

declare function gn:for-each($gen as f:generator, $fun as function(*))
{
  let $gen := if(not($gen?initialized)) then $gen?moveNext()
                else $gen        
   return
     if($gen?endReached) then gn:emptyGenerator($gen)
      else
       let $current := $fun($gen?getCurrent()),
            $newResultGen := map:put($gen, "getCurrent", %method fn(){$current}),
            $nextGen := $gen?moveNext()
        return
          if($nextGen?endReached) then $newResultGen
            else
              let $newResultGen2 :=  map:put($newResultGen, "moveNext", %method fn() {gn:for-each($nextGen, $fun)}) 
                 return
                   $newResultGen2         
};

declare function gn:for-each-pair($gen as f:generator, $gen2 as f:generator, $fun as function(*))
{
  let $gen := if(not($gen?initialized)) then $gen?moveNext()
              else $gen,
      $gen2 := if(not($gen2?initialized)) then $gen2?moveNext()
              else $gen2
   return
      if($gen?endReached or $gen2?endReached) then gn:emptyGenerator($gen) 
       else  
         let $current := $fun($gen?getCurrent(), $gen2?getCurrent()),
             $newResultGen := map:put($gen, "getCurrent", %method fn(){$current}),
             $nextGen1 := $gen?moveNext(),
             $nextGen2 := $gen2?moveNext()
          return
             if($nextGen1?endReached or $nextGen2?endReached) then $newResultGen
               else
                 let $newResultGen2 := map:put($newResultGen, "moveNext", %method fn(){gn:for-each-pair($nextGen1, $nextGen2, $fun)})
                   return
                     $newResultGen2      
};

declare function gn:zip($gen as f:generator, $gen2 as f:generator)
{
  gn:for-each-pair($gen, $gen2, fn($x1, $x2){[$x1, $x2]})
};

declare function gn:concat($gen as f:generator, $gen2 as f:generator)
      {
        let $gen := if(not($gen?initialized)) then $gen?moveNext()
                    else $gen,
            $gen2 := if(not($gen2?initialized)) then $gen2?moveNext()
                    else $gen2,
            $resultGen := if($gen?endReached) then $gen2
                            else if($gen2?endReached) then $gen
                            else
                              $gen  => map:put( "moveNext", 
                                                %method fn()
                                                 {
                                                 let $nextGen := $gen?moveNext()
                                                   return 
                                                     gn:concat($nextGen, $gen2)
                                                 }
                                              )                                   
        return 
           $resultGen            
      };
      
declare function gn:append($gen as f:generator, $value as item()*)
      {
        let $gen := if(not($gen?initialized)) then $gen?moveNext()
                    else $gen,
            $genSingle := $gen => map:put("getCurrent", %method fn(){$value})
                               => map:put("moveNext", %method fn(){gn:emptyGenerator($gen)})
         return
           gn:concat($gen, $genSingle)                    
      };  
      
declare function gn:prepend($gen as f:generator, $value as item()*)
      {
        let $gen := if(not($gen?initialized)) then $gen?moveNext()
            else $gen,
            $genSingle := $gen => map:put("getCurrent", %method fn(){$value})
                               => map:put("moveNext", %method fn(){gn:emptyGenerator($gen)})
         return
           gn:concat($genSingle, $gen)  
      };    
      
declare function gn:insertAt($gen as f:generator, $pos as xs:positiveInteger, $value as item()*)
      {
        let $genTail := gn:skipStrict($gen, $pos - 1, true())
         return
            if($pos gt 1)
              then gn:concat(gn:append(gn:take($gen, $pos - 1), $value), $genTail)
              else gn:prepend($genTail, $value)               
      };     
      
declare function gn:removeAt($gen as f:generator, $pos as xs:nonNegativeInteger)
      {
        let $genTail := gn:skipStrict($gen, $pos, true())
          return
            if($pos gt 1)
              then gn:concat(gn:take($gen, $pos - 1), $genTail)
              else $genTail
      };      
     
declare function gn:replace($gen as f:generator, $funIsMatching as function(item()*) as xs:boolean, $replacement as item()*)
      {
        if($gen?endReached) then $gen
          else
            let $current := $gen?getCurrent()
              return
                if($funIsMatching($current))
                  then let $nextGen := $gen?moveNext()
                     return
                       $gen => map:put("getCurrent", %method fn() {$replacement})
                            => map:put("moveNext", %method fn() { $nextGen } 
                                  )
                  else (: $current is not the match for replacement :)
                    let $nextGen := $gen?moveNext()
                      return $gen => map:put("moveNext", 
                                           %method fn()
                                           {
                                             let $intendedReplace := function($z) {$z?replace($funIsMatching, $replacement)}
                                              return
                                                if($nextGen?endReached) then $nextGen
                                                else $intendedReplace($nextGen)
                                           }
                                        )
      };      
      
declare function gn:filter($gen as f:generator, $pred as function(item()*) as xs:boolean)
{
 if($gen?initialized and $gen?endReached) then gn:emptyGenerator($gen)
  else
    let $getNextGoodGen := function($gen as map(*), 
                                 $pred as function(item()*) as xs:boolean)
       {
          if($gen?endReached) then gn:emptyGenerator($gen)
          else
            let $mapResult := 
                  while-do(
                           $gen,
                           function($x) { not($x?endReached) and not($pred($x?getCurrent()))},
                           function($x) { $x?moveNext() }
                           )   
            return 
              if($mapResult?endReached) then gn:emptyGenerator($gen)
               else $mapResult                  
       },
       
       $gen := if($gen?initialized) then $gen 
                 else $gen?moveNext(),
       $nextGoodGen := $getNextGoodGen($gen, $pred)
    return
      if($nextGoodGen?endReached) then gn:emptyGenerator($gen)
      else
        $nextGoodGen => map:put("moveNext", 
                                %method fn() 
                                  {
                                    let $nextGoodGen := $getNextGoodGen(?inputGen?moveNext(), $pred)
                                      return
                                        if($nextGoodGen?endReached) then gn:emptyGenerator($nextGoodGen)
                                        else
                                          map:put(map:put($nextGoodGen, "moveNext", %method fn() {gn:filter($nextGoodGen?moveNext(), $pred)}),
                                                          "inputGen", $nextGoodGen
                                                  )
                                   }
                               )
                       => map:put("inputGen", $nextGoodGen)
  }; 
  
declare function gn:fold-left($gen as f:generator, $init as item()*, $action as fn(*))
{
  if($gen?endReached) then $init
    else gn:fold-left(gn:tail($gen), $action($init, $gen?getCurrent()), $action)
};

declare function gn:fold-right($gen as f:generator, $init as item()*, $action as fn(*))
{
  if($gen?endReached) then $init
    else $action(gn:head($gen), gn:fold-right(gn:tail($gen), $init, $action))
};

declare function gn:fold-lazy($gen as f:generator, $init as item()*, $action as fn(*), $shortCircuitProvider as function(*))
{
  if($gen?endReached) then $init
  else
   let $current := $gen?getCurrent()
     return
       if(function-arity($shortCircuitProvider($current, $init)) eq 0)
         then $shortCircuitProvider($current, $init)()
         else $action($current, gn:fold-lazy($gen?moveNext(), $init, $action, $shortCircuitProvider))
};

declare function gn:makeGenerator($gen as f:generator, $provider as function(*))
{
 let $gen := if(not($gen?initialized)) then $gen?moveNext()
            else $gen,
      $nextDataItemGetter := $provider(0),
      $nextGen := if(not($nextDataItemGetter instance of function(*))) then gn:emptyGenerator($gen)  
                   else gn:emptyGenerator($gen)
                    => map:put("numDataItems", 1)
                    => map:put("current", $nextDataItemGetter())
                    => map:put("endReached", false())
                    => map:put("getCurrent", %method fn() {?current})
                    => map:put("moveNext",  
                               %method fn() 
                                {
                                  let $nextDataItemGetter := $provider(?numDataItems)
                                    return
                                      if(not($nextDataItemGetter instance of function(*))) then gn:emptyGenerator($gen)
                                      else
                                        . => map:put("current", $nextDataItemGetter())
                                          => map:put("numDataItems", ?numDataItems + 1)
                                }
                               )
   return $nextGen                                                  
};   

declare function gn:makeGeneratorFromArray($gen as f:generator, $input as array(*))
{
  let $size := array:size($input),
      $arrayProvider := fn($ind as xs:integer)
                        {
                          if($ind +1 gt $size) then -1
                           else fn(){$input($ind + 1)}
                        }
   return gn:makeGenerator($gen, $arrayProvider)
};  

declare function gn:makeGeneratorFromSequence($gen as f:generator, $input as item()*)
{
  let $size := count($input),
      $seqProvider := fn($ind as xs:integer)
                        {
                          if($ind +1 gt $size) then -1
                           else fn(){$input[$ind + 1]}
                        }
   return gn:makeGenerator($gen, $seqProvider)
};   

declare function gn:toSequence($gen as f:generator) {gn:toArray($gen) => array:items()};     

declare function gn:emptyGenerator($gen as f:generator) 
{
  $gen => map:put("initialized", true()) => map:put("endReached", true())
    => map:put("getCurrent", %method fn() {error((),"getCurrent() called on an emptyGenerator")})
    => map:put("moveNext", %method fn() {error((),"moveNext() called on an emptyGenerator")})
};         

declare record f:generator 
   ( initialized as xs:boolean,
     endReached as xs:boolean,
     getCurrent as %method fn() as item()*,
     moveNext as %method fn(*) (: as f:generator, :),
     toArray := %method fn()
     {
        gn:toArray(.)
     },
     
     take := %method fn($n as xs:integer) 
     {
        gn:take(., $n)
     },
      
      takeWhile := %method fn($pred as function(item()*) as xs:boolean)
      {
         gn:takeWhile(., $pred) 
      },
     
     skipStrict := %method fn($n as xs:integer, $issueErrorOnEmpty as xs:boolean) 
     {
        gn:skipStrict(., $n, $issueErrorOnEmpty)
     },
     skip := %method fn($n as xs:integer) 
     {
       gn:skip(., $n)
     },
     
     skipWhile := %method fn($pred as function(item()*) as xs:boolean)
     {
       gn:skipWhile(., $pred)                 
     },
     
     some := %method fn()
     {
       gn:some(.)
     },
     
     someWhere := %method fn($pred)
     {
       gn:someWhere(., $pred)
     },
     
     subrange := %method fn($m as xs:integer, $n as xs:integer)
     {
       gn:subrange(., $m, $n)
     },
     
     chunk := %method fn($size as xs:integer)
     {
       gn:chunk(., $size)
     },
     
     head := %method fn() {gn:head(.)},
     tail := %method fn() {gn:tail(.)},
     
     at := %method fn($ind) {gn:at(., $ind)},
           
     for-each := %method fn($fun as function(*))
     {
       gn:for-each(., $fun)                 
      },
      
      for-each-pair := %method fn($gen2 as f:generator, $fun as function(*))
      {
        gn:for-each-pair(., $gen2, $fun)                    
      },
      
      zip := %method fn($gen2 as f:generator)
      {
        gn:for-each-pair(., $gen2, fn($x1, $x2){[$x1, $x2]})
      },

      concat := %method fn($gen2 as f:generator)
      {
        gn:concat(., $gen2)           
      },

      append := %method fn($value as item()*)
      {
        gn:append(., $value)                    
      },
      
      prepend := %method fn($value as item()*)
      {
        gn:prepend(., $value)
      },
      
      insertAt := %method fn($pos as xs:nonNegativeInteger, $value as item()*)
      {
        gn:insertAt(., $pos, $value)               
      },
      
      removeAt := %method fn($pos as xs:nonNegativeInteger)
      {
        gn:removeAt(., $pos)
      },
      
      replace := %method fn($funIsMatching as function(item()*) as xs:boolean, $replacement as item()*)
      {
        gn:replace(., $funIsMatching, $replacement)                  
      },

      filter := %method fn($pred as function(item()*) as xs:boolean)
      {
        gn:filter(., $pred)
      },   
      
      fold-left := %method fn($init as item()*, $action as fn(*))
      {
        gn:fold-left(., $init, $action)
      },
        
      fold-right := %method fn($init as item()*, $action as fn(*))
      {
        gn:fold-right(., $init, $action)
      },
      
      fold-lazy := %method fn($init as item()*, $action as fn(*), $shortCircuitProvider as function(*))
      {
        gn:fold-lazy(., $init, $action, $shortCircuitProvider)
      },
        
      makeGenerator := %method fn($provider as function(*))
      {
        gn:makeGenerator(., $provider)                                             
      },
        
        makeGeneratorFromArray := %method fn($input as array(*))
        {
          gn:makeGeneratorFromArray(., $input)
        },
        
        makeGeneratorFromSequence := %method fn($input as item()*)
        {
          gn:makeGeneratorFromSequence(., $input)
        },
        
        toSequence := %method fn() {gn:toSequence(.)},     
        
        emptyGenerator := %method fn() 
        {
          gn:emptyGenerator(.)
        },      
     *
   );

let $gen2ToInf := f:generator(initialized := true(), endReached := false(), 
                              getCurrent := %method fn(){?last +1},
                              moveNext := %method fn()
                              {
                                if(not(?initialized))
                                  then map:put(., "inittialized", true())
                                  else map:put(., "last", ?last + 1)
                              },
                              options := {"last" : 1}
                             ),
    $double := fn($n) {2*$n},
    $sum2 := fn($m, $n) {$m + $n},
    $product := fn($m, $n) {$m * $n}    
  return    
  (
    "$gen2ToInf?take(3)?toArray()",
    $gen2ToInf?take(3)?toArray(),
    "================",    
    "$gen2ToInf?take(3)?skip(2)?getCurrent()",
    $gen2ToInf?take(3)?skip(2)?getCurrent(),
    (: $gen2ToInf?take(3)?moveNext()?moveNext()?moveNext()?getCurrent(), :)
    "================",
    "$gen2ToInf?getCurrent()",
    $gen2ToInf?getCurrent(),
    "$gen2ToInf?moveNext()?getCurrent()",
    $gen2ToInf?moveNext()?getCurrent(),
    "================",
    "$gen2ToInf?take(5) instance of f:generator",
    $gen2ToInf?take(5) instance of f:generator,
    "==>  $gen2ToInf?skip(7) instance of f:generator",
    $gen2ToInf?skip(7) instance of f:generator,  
    "================",
    "$gen2ToInf?subrange(4, 6)?getCurrent()",
    $gen2ToInf?subrange(4, 6)?getCurrent(), 
    "$gen2ToInf?subrange(4, 6)?moveNext()?getCurrent()",
    $gen2ToInf?subrange(4, 6)?moveNext()?getCurrent(),
    "$gen2ToInf?subrange(4, 6)?moveNext()?moveNext()?getCurrent()",
    $gen2ToInf?subrange(4, 6)?moveNext()?moveNext()?getCurrent(),
    (: $gen2ToInf?subrange(4, 6)?moveNext()?moveNext()?moveNext()?getCurrent() :) (: Must raise error:)    
    "================",    
    "$gen2ToInf?subrange(4, 6)?head()",
    $gen2ToInf?subrange(4, 6)?head(),  
    "$gen2ToInf?subrange(4, 6)?tail()?head()",
    $gen2ToInf?subrange(4, 6)?tail()?head(),
    "$gen2ToInf?subrange(4, 6)?toArray()",
    $gen2ToInf?subrange(4, 6)?toArray(),
    "$gen2ToInf?head()",
    $gen2ToInf?head(),
    "==>  $gen2ToInf?tail()?head()",
    $gen2ToInf?tail()?head(),
    "================", 
    "$gen2ToInf?subrange(4, 6)?tail()?toArray()",
    $gen2ToInf?subrange(4, 6)?tail()?toArray(),
    "================",
    "$gen2ToInf?at(5)",
    $gen2ToInf?at(5), 
    "================",
    "$gen2ToInf?subrange(1, 5)?toArray()",
    $gen2ToInf?subrange(1, 5)?toArray(),
    "$gen2ToInf?subrange(1, 5)?for-each($double)?toArray()",
    $gen2ToInf?subrange(1, 5)?for-each($double)?toArray(),
    "$gen2ToInf?take(5)?for-each($double)?toArray()",
    $gen2ToInf?take(5)?for-each($double)?toArray(),
    "==>  $gen2ToInf?for-each($double)?take(5)?toArray()",
    $gen2ToInf?for-each($double)?take(5)?toArray(),
    "================",
    "$gen2ToInf?subrange(1, 5)?toArray()",
    $gen2ToInf?subrange(1, 5)?toArray(),
    "$gen2ToInf?subrange(6, 10)?toArray()",
    $gen2ToInf?subrange(6, 10)?toArray(),
    "$gen2ToInf?subrange(1, 5)?for-each-pair($gen2ToInf?subrange(6, 10), $sum2)?toArray()",
    $gen2ToInf?subrange(1, 5)?for-each-pair($gen2ToInf?subrange(6, 10), $sum2)?toArray(), 
    "==>  $gen2ToInf?for-each-pair($gen2ToInf, $sum2)?take(5)?toArray()",
    $gen2ToInf?for-each-pair($gen2ToInf, $sum2)?take(5)?toArray(),
    "================",
    "==>  $gen2ToInf?filter(fn($n){$n mod 2 eq 1})?getCurrent()",
    $gen2ToInf?filter(fn($n){$n mod 2 eq 1})?getCurrent(),
    "$gen2ToInf?filter(fn($n){$n mod 2 eq 1})?moveNext()?getCurrent()",
    $gen2ToInf?filter(fn($n){$n mod 2 eq 1})?moveNext()?getCurrent(),
    "================", 
    "$gen2ToInf?filter(fn($n){$n mod 2 eq 1})?take(10)?toArray()",
    $gen2ToInf?filter(fn($n){$n mod 2 eq 1})?take(10)?toArray(),  
    "================", 
    "$gen2ToInf?filter(fn($n){$n mod 2 eq 1})?take(10)?toSequence()",
    $gen2ToInf?filter(fn($n){$n mod 2 eq 1})?take(10)?toSequence(),
    "================", 
    "$gen2ToInf?takeWhile(fn($n){$n < 11})?toArray()",
    $gen2ToInf?takeWhile(fn($n){$n < 11})?toArray(), 
    "$gen2ToInf?takeWhile(fn($n){$n < 2})?toArray()",
    $gen2ToInf?takeWhile(fn($n){$n < 2})?toArray(), 
    "================", 
    "$gen2ToInf?skipWhile(fn($n){$n < 11})?take(5)?toArray()",
    $gen2ToInf?skipWhile(fn($n){$n < 11})?take(5)?toArray(),
    "==> $gen2ToInf?skipWhile(fn($n){$n < 2})",
    $gen2ToInf?skipWhile(fn($n){$n < 2}),
    "
     ==> $gen2ToInf?skipWhile(fn($n){$n < 2})?skip(1)",
    $gen2ToInf?skipWhile(fn($n){$n < 2})?skip(1),
(:    $gen2ToInf?skipWhile(fn($x) {$x ge 2}) :) (: ?skip(1) :)
    "================", 
    "$gen2ToInf?some()",
     $gen2ToInf?some(),
     "let $empty := $gen2ToInf?emptyGenerator()
      return $empty?some()",
     let $empty := $gen2ToInf?emptyGenerator()
      return $empty?some(),
    "================",
    "$gen2ToInf?take(5)?filter(fn($n){$n ge 7})?some()",
     $gen2ToInf?take(5)?filter(fn($n){$n ge 7})?some(),  
     "$gen2ToInf?take(5)?someWhere(fn($n){$n ge 7})",
     $gen2ToInf?take(5)?someWhere(fn($n){$n ge 7}), 
     "$gen2ToInf?take(5)?someWhere(fn($n){$n ge 6})",
     $gen2ToInf?take(5)?someWhere(fn($n){$n ge 6}),
     "$gen2ToInf?someWhere(fn($n){$n ge 100})",
     $gen2ToInf?someWhere(fn($n){$n ge 100}),
     "================",
     "$gen2ToInf?take(10)?take(11)?toArray()",
     $gen2ToInf?take(10)?take(11)?toArray(),
     "$gen2ToInf?take(10)?skip(10)?toArray()",
     $gen2ToInf?take(10)?skip(10)?toArray(),
     "$gen2ToInf?take(10)?skip(9)?toArray()",     
     $gen2ToInf?take(10)?skip(9)?toArray(),
     "$gen2ToInf?take(10)?subrange(3, 12)?toArray()",
     $gen2ToInf?take(10)?subrange(3, 12)?toArray(),
     "$gen2ToInf?take(10)?subrange(5, 3)?toArray()",
     $gen2ToInf?take(10)?subrange(5, 3)?toArray(),
     "================",
     "$gen2ToInf?take(100)?chunk(20)?getCurrent()",
      $gen2ToInf?take(100)?chunk(20)?getCurrent(),
      "==>  $gen2ToInf?chunk(20)?take(5)?toArray()",
      $gen2ToInf?chunk(20)?take(5)?toArray(),
     "================",
     "$gen2ToInf?take(100)?chunk(20)?moveNext()?getCurrent()",
      $gen2ToInf?take(100)?chunk(20)?moveNext()?getCurrent(),
     "$gen2ToInf?take(100)?chunk(20)?moveNext()?moveNext()?getCurrent()", 
      $gen2ToInf?take(100)?chunk(20)?moveNext()?moveNext()?getCurrent(),
     "$gen2ToInf?take(100)?chunk(20)?skip(1)?getCurrent()",      
      $gen2ToInf?take(100)?chunk(20)?skip(1)?getCurrent(),
     "================",      
     "$gen2ToInf?take(100)?chunk(20)?for-each(fn($genX){$genX})?toArray()",      
      $gen2ToInf?take(100)?chunk(20)?for-each(fn($genX){$genX})?toArray(),
     "================",  
     "$gen2ToInf?take(10)?chunk(4)?toArray()",
      $gen2ToInf?take(10)?chunk(4)?toArray(),
      "$gen2ToInf?take(10)?chunk(4)?for-each(fn($arr){array:size($arr)})?toArray()",
      $gen2ToInf?take(10)?chunk(4)?for-each(fn($arr){array:size($arr)})?toArray(),
     "================", 
     "$gen2ToInf?subrange(10, 15)?concat($gen2ToInf?subrange(1, 9))?toArray()",
     $gen2ToInf?subrange(10, 15)?concat($gen2ToInf?subrange(1, 9))?toArray(),
     "================", 
     "$gen2ToInf?subrange(1, 5)?append(101)?toArray()",
     $gen2ToInf?subrange(1, 5)?append(101)?toArray(),
     "$gen2ToInf?subrange(1, 5)?prepend(101)?toArray()",
     $gen2ToInf?subrange(1, 5)?prepend(101)?toArray(),
     "==>  $gen2ToInf?append(101)",
     $gen2ToInf?append(101),
     "$gen2ToInf?prepend(101)?take(5)?toArray()",
     $gen2ToInf?prepend(101)?take(5)?toArray(),
     "================", 
     "$gen2ToInf?subrange(1, 5)?zip($gen2ToInf?subrange(6, 10))?toArray()",
     $gen2ToInf?subrange(1, 5)?zip($gen2ToInf?subrange(6, 10))?toArray(),
     "$gen2ToInf?subrange(1, 5)?zip($gen2ToInf?subrange(10, 20))?toArray()",
     $gen2ToInf?subrange(1, 5)?zip($gen2ToInf?subrange(10, 20))?toArray(),
     "==>  $gen2ToInf?zip($gen2ToInf?skip(5))?take(10)?toArray()",
     $gen2ToInf?zip($gen2ToInf?skip(5))?take(10)?toArray(),
     "================", 
     "$gen2ToInf?makeGenerator(fn($numGenerated as xs:integer)
                                 {if($numGenerated le 9) then fn() {$numGenerated + 1} else -1} 
                             )?toArray()",
     $gen2ToInf?makeGenerator(fn($numGenerated as xs:integer)
                                 {if($numGenerated le 9) then fn() {$numGenerated + 1} else -1} 
                             )?toArray(),
     "================", 
     "$gen2ToInf?makeGeneratorFromArray([1, 4, 9, 16, 25])?toArray()",
      $gen2ToInf?makeGeneratorFromArray([1, 4, 9, 16, 25])?toArray(),
      "$gen2ToInf?makeGeneratorFromSequence((1, 8, 27, 64, 125))?toArray()",
      $gen2ToInf?makeGeneratorFromSequence((1, 8, 27, 64, 125))?toArray(), 
     "================", 
     "$gen2ToInf?take(10)?insertAt(3, ""XYZ"")?toArray()",
      $gen2ToInf?take(10)?insertAt(3, "XYZ")?toArray(),
      "$gen2ToInf?take(10)?insertAt(1, ""ABC"")?toArray()",
      $gen2ToInf?take(10)?insertAt(1, "ABC")?toArray(),
      "$gen2ToInf?take(10)?insertAt(11, ""PQR"")?toArray()",
      $gen2ToInf?take(10)?insertAt(11, "PQR")?toArray(),
      "==>  $gen2ToInf?insertAt(3, ""XYZ"")?take(10)?toArray()", 
      $gen2ToInf?insertAt(3, "XYZ")?take(10)?toArray(),
     (: , $gen2ToInf?take(10)?insertAt(12, "GHI")?toArray() :)  (:  Must raise error "Input Generator too-short." :) 
     "================", 
     "$gen2ToInf?take(10)?removeAt(3)?toArray()",
      $gen2ToInf?take(10)?removeAt(3)?toArray(),
      "$gen2ToInf?take(10)?removeAt(1)?toArray()",
      $gen2ToInf?take(10)?removeAt(1)?toArray(),
      "$gen2ToInf?take(10)?removeAt(10)?toArray()",
      $gen2ToInf?take(10)?removeAt(10)?toArray(),
      "==>  $gen2ToInf?removeAt(3)?take(10)?toArray()",
      $gen2ToInf?removeAt(3)?take(10)?toArray(),
      (: , $gen2ToInf?take(10)?removeAt(11)?toArray() :)        (:  Must raise error "Input Generator too-short." :) 
     "================",
     "$gen2ToInf?take(10)?replace(fn($x){$x gt 4}, ""Replacement"")?toArray()",
      $gen2ToInf?take(10)?replace(fn($x){$x gt 4}, "Replacement")?toArray(),
      "$gen2ToInf?take(10)?replace(fn($x){$x lt 3}, ""Replacement"")?toArray()",
      $gen2ToInf?take(10)?replace(fn($x){$x lt 3}, "Replacement")?toArray(),
      "$gen2ToInf?take(10)?replace(fn($x){$x gt 10}, ""Replacement"")?toArray()",
      $gen2ToInf?take(10)?replace(fn($x){$x gt 10}, "Replacement")?toArray(),
      "$gen2ToInf?take(10)?replace(fn($x){$x gt 11}, ""Replacement"")?toArray()",
      $gen2ToInf?take(10)?replace(fn($x){$x gt 11}, "Replacement")?toArray(),
      "$gen2ToInf?take(10)?replace(fn($x){$x lt 2}, ""Replacement"")?toArray()",
      $gen2ToInf?take(10)?replace(fn($x){$x lt 2}, "Replacement")?toArray(),
      "$gen2ToInf?replace(fn($x){$x gt 4}, ""Replacement"")?take(10)?toArray()",
      $gen2ToInf?replace(fn($x){$x gt 4}, "Replacement")?take(10)?toArray(),
      "$gen2ToInf?replace(fn($x){$x lt 3}, ""Replacement"")?take(10)?toArray()",
      $gen2ToInf?replace(fn($x){$x lt 3}, "Replacement")?take(10)?toArray(),
      (:
      , "==>  ==>  ==>  $gen2ToInf?replace2(fn($x){$x lt 2}, ""Replacement"")?take(10)?toArray() <==  <==  <==",
      $gen2ToInf?replace2(fn($x){$x lt 2}, "Replacement")?take(10)?toArray() 
      :)
    "================",
    "$gen2ToInf?take(5)?fold-left(0, fn($x, $y){$x + $y})",
    $gen2ToInf?take(5)?fold-left(0, fn($x, $y){$x + $y}),
    "================",
    "$gen2ToInf?take(5)?fold-right(0, fn($x, $y){$x + $y})",
    $gen2ToInf?take(5)?fold-right(0, fn($x, $y){$x + $y}),
    "================",
    let $multShortCircuitProvider := fn($x, $y)
        {
          if($x eq 0) then fn(){0}
            else fn($z) {$x * $z}
        },
        $gen-5ToInf := $gen2ToInf?for-each(fn($n){$n -7})
     return
     (
       "let $multShortCircuitProvider := fn($x, $y)
        {
          if($x eq 0) then fn(){0}
            else fn($z) {$x * $z}
        },
            $gen-5ToInf := $gen2ToInf?for-each(fn($n){$n -7})
          return
            $gen2ToInf?take(5)?fold-lazy(1, $product, $multShortCircuitProvider),
            $gen-5ToInf?fold-lazy(1, $product, $multShortCircuitProvider)",
       $gen2ToInf?take(5)?fold-lazy(1, $product, $multShortCircuitProvider),
       $gen-5ToInf?fold-lazy(1, $product, $multShortCircuitProvider)
     )              
   )
