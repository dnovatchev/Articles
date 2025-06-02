declare namespace f = "http://www.w3.org/2005/xpath-functions-2025";
declare namespace gn = "http://www.w3.org/2005/xpath-functions-2025/generator";
declare function gn:to-array($gen as f:generator) as array(*)
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

declare function gn:take($gen as f:generator, $n as xs:integer) as f:generator
{
  let $gen := if(not($gen?initialized)) then $gen?moveNext()
                else $gen
   return
     if($gen?endReached or $n le 0) then gn:empty-generator($gen)
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

declare function gn:take-while($gen as f:generator, $pred as function(item()*) as xs:boolean) as f:generator
{
  let $gen := if(not($gen?initialized)) then $gen?moveNext()
                else $gen
   return
     if($gen?endReached) then gn:empty-generator($gen)
      else      
        let $current := $gen?getCurrent()
          return
            if(not($pred($current))) then gn:empty-generator($gen)
            else
              let $newResultGen := map:put($gen, "getCurrent", %method fn(){$current}),
                  $nextGen := $gen?moveNext()
               return
                  if($nextGen?endReached) then $newResultGen
                  else
                    let $newResultGen2 :=  map:put($newResultGen, "moveNext", %method fn() {gn:take-while($nextGen, $pred)}) 
                     return $newResultGen2    
};

declare function gn:skip-strict($gen as f:generator, $n as xs:nonNegativeInteger, $issueErrorOnEmpty as xs:boolean) as f:generator
{
  if($n eq 0) then $gen
    else if($gen?endReached) 
           then if($issueErrorOnEmpty)
                 then error((), "Input Generator too-short") 
                 else gn:empty-generator($gen)
    else 
      let $gen := if(not($gen?initialized)) then $gen?moveNext()
                   else $gen
        return
          if(not($gen?endReached)) then gn:skip-strict($gen?moveNext(), $n -1, $issueErrorOnEmpty)
            else gn:empty-generator($gen)    
};

declare function gn:skip($gen as f:generator, $n as xs:nonNegativeInteger) as f:generator
{
  gn:skip-strict($gen, $n, false())
};

declare function gn:skip-while($gen as f:generator, $pred as function(item()*) as xs:boolean) as f:generator
{
  let $gen := if(not($gen?initialized)) then $gen?moveNext()
                else $gen
   return
     if($gen?endReached) then gn:empty-generator($gen)
      else
        let $current := $gen?getCurrent()
         return
           if(not($pred($current))) then $gen
            else gn:skip-while($gen?moveNext(), $pred)  
};

declare function gn:subrange($gen as f:generator, $m as xs:positiveInteger, $n as xs:positiveInteger) as f:generator
{
 gn:take(gn:skip($gen, $m - 1), $n - $m + 1)  
};

declare function gn:some($gen as f:generator) as xs:boolean
{
 $gen?initialized and not($gen?endReached)  
};

declare function gn:some-where($gen as f:generator, $pred) as xs:boolean
{
 gn:some(gn:filter($gen, $pred))
};

declare function gn:first-where($gen as f:generator, $pred) as item()*
{
 gn:head(gn:filter($gen, $pred))
};

declare function gn:chunk($gen as f:generator, $size as xs:positiveInteger) as f:generator
{
  let $gen := if(not($gen?initialized)) then $gen?moveNext()
                else $gen
   return
     if($gen?endReached) then gn:empty-generator($gen)
     else
       let $thisChunk := gn:to-array(gn:take($gen, $size)),
           $cutGen := gn:skip($gen, $size),
           $resultGen := $gen => map:put("getCurrent", %method fn(){$thisChunk})
                              => map:put("moveNext", %method fn(){gn:chunk($cutGen, $size)})
        return $resultGen  
};

declare function gn:head($gen as f:generator) as item()* {gn:take($gen, 1)?getCurrent()};

declare function gn:tail($gen as f:generator) as f:generator {gn:skip($gen, 1)};

declare function gn:at($gen as f:generator, $ind) as item()* {gn:subrange($gen, $ind, $ind)?getCurrent()};

declare function gn:contains($gen as f:generator, $value as item()*) as xs:boolean
     {
       let $gen := if(not($gen?initialized)) then ?moveNext()
                     else $gen
        return
          if($gen?endReached) then false()
           else
             let $current := $gen?getCurrent()
               return
                  if(deep-equal($current, $value)) then true()
                   else gn:contains($gen?moveNext(), $value) 
     };

declare function gn:for-each($gen as f:generator, $fun as function(*)) as f:generator
{
  let $gen := if(not($gen?initialized)) then $gen?moveNext()
                else $gen        
   return
     if($gen?endReached) then gn:empty-generator($gen)
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

declare function gn:for-each-pair($gen as f:generator, $gen2 as f:generator, $fun as function(*)) as f:generator
{
  let $gen := if(not($gen?initialized)) then $gen?moveNext()
              else $gen,
      $gen2 := if(not($gen2?initialized)) then $gen2?moveNext()
              else $gen2
   return
      if($gen?endReached or $gen2?endReached) then gn:empty-generator($gen) 
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

declare function gn:zip($gen as f:generator, $gen2 as f:generator) as f:generator
{
  gn:for-each-pair($gen, $gen2, fn($x1, $x2){[$x1, $x2]})
};

declare function gn:concat($gen as f:generator, $gen2 as f:generator) as f:generator
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
      
declare function gn:append($gen as f:generator, $value as item()*) as f:generator
      {
        let $gen := if(not($gen?initialized)) then $gen?moveNext()
                    else $gen,
            $genSingle := $gen => map:put("getCurrent", %method fn(){$value})
                               => map:put("moveNext", %method fn(){gn:empty-generator($gen)})
                               => map:put("endReached", false())
         return
           gn:concat($gen, $genSingle)                    
      };  
      
declare function gn:prepend($gen as f:generator, $value as item()*) as f:generator
      {
        let $gen := if(not($gen?initialized)) then $gen?moveNext()
            else $gen,
            $genSingle := $gen => map:put("getCurrent", %method fn(){$value})
                               => map:put("moveNext", %method fn(){gn:empty-generator($gen)})
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
        let $gen := if(not($gen?initialized)) then $gen?moveNext()
                      else $gen
          return
            gn:filter($gen, fn($x){not($predicate($x))})  
      };      

declare function gn:distinct($gen as f:generator) as f:generator
      {
        let $gen := if(not($gen?initialized)) then $gen?moveNext()
                      else $gen
         return
           if($gen?endReached) then $gen
           else
             let $priorValue := $gen?getCurrent()
               return
                 $gen => map:put("moveNext", %method fn(){gn:distinct(gn:remove-where(gn:tail($gen), fn($x){deep-equal($priorValue, $x)}))})  
      };
     
declare function gn:replace($gen as f:generator, $funIsMatching as function(item()*) as xs:boolean, $replacement as item()*) as f:generator
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
      
declare function gn:reverse($gen as f:generator) as f:generator
{
  if($gen?endReached) then gn:empty-generator($gen)
    else
     let $current := $gen?getCurrent()
       return
         gn:append(gn:reverse(gn:tail($gen)), $current)
};      
      
declare function gn:filter($gen as f:generator, $pred as function(item()*) as xs:boolean) as f:generator
{
 if($gen?initialized and $gen?endReached) then gn:empty-generator($gen)
  else
    let $getNextGoodGen := function($gen as map(*), 
                                 $pred as function(item()*) as xs:boolean)
       {
          if($gen?endReached) then gn:empty-generator($gen)
          else
            let $mapResult := 
                  while-do(
                           $gen,
                           function($x) { not($x?endReached) and not($pred($x?getCurrent()))},
                           function($x) { $x?moveNext() }
                           )   
            return 
              if($mapResult?endReached) then gn:empty-generator($gen)
               else $mapResult                  
       },
       
       $gen := if($gen?initialized) then $gen 
                 else $gen?moveNext(),
       $nextGoodGen := $getNextGoodGen($gen, $pred)
    return
      if($nextGoodGen?endReached) then gn:empty-generator($gen)
      else
        $nextGoodGen => map:put("moveNext", 
                                %method fn() 
                                  {
                                    let $nextGoodGen := $getNextGoodGen(?inputGen?moveNext(), $pred)
                                      return
                                        if($nextGoodGen?endReached) then gn:empty-generator($nextGoodGen)
                                        else
                                          map:put(map:put($nextGoodGen, "moveNext", %method fn() {gn:filter($nextGoodGen?moveNext(), $pred)}),
                                                          "inputGen", $nextGoodGen
                                                  )
                                   }
                               )
                       => map:put("inputGen", $nextGoodGen)
  }; 
  
declare function gn:fold-left($gen as f:generator, $init as item()*, $action as fn(*)) as item()*
{
  if($gen?endReached) then $init
    else gn:fold-left(gn:tail($gen), $action($init, $gen?getCurrent()), $action)
};

declare function gn:fold-right($gen as f:generator, $init as item()*, $action as fn(*)) as item()*
{
  if($gen?endReached) then $init
    else $action(gn:head($gen), gn:fold-right(gn:tail($gen), $init, $action))
};

declare function gn:fold-lazy($gen as f:generator, $init as item()*, $action as fn(*), $shortCircuitProvider as function(*)) as item()*
{
  if($gen?endReached) then $init
  else
   let $current := $gen?getCurrent()
     return
       if(function-arity($shortCircuitProvider($current, $init)) eq 0)
         then $shortCircuitProvider($current, $init)()
         else $action($current, gn:fold-lazy($gen?moveNext(), $init, $action, $shortCircuitProvider))
};

declare function gn:scan-left($gen as f:generator, $init as item()*, $action as fn(*)) as f:generator
{
  let $resultGen := $gen?empty-generator() 
                        => map:put("endReached", false())
                        => map:put("getCurrent", %method fn(){$init})
   return
     if($gen?endReached) 
       then $resultGen => map:put("moveNext", %method fn(){$gen?empty-generator()})
       else
         let $resultGen := $resultGen => map:put("getCurrent", %method fn(){$init}),
             $partialFoldResult := $action($init, $gen?getCurrent())
           return
             let $nextGen := $gen?moveNext()
              return
                $resultGen => map:put("moveNext", %method fn()
                                      { 
                                          gn:scan-left($nextGen, $partialFoldResult, $action)
                                       }
                                      )            
};

declare function gn:scan-right($gen as f:generator, $init as item()*, $action as fn(*)) as f:generator
{
  gn:reverse(gn:scan-left(gn:reverse($gen), $init, $action))                         
};

declare function gn:make-generator($gen as f:generator, $provider as function(*)) as f:generator
{
 let $gen := if(not($gen?initialized)) then $gen?moveNext()
            else $gen,
      $nextDataItemGetter := $provider(0),
      $nextGen := if(not($nextDataItemGetter instance of function(*))) then gn:empty-generator($gen)  
                   else gn:empty-generator($gen)
                    => map:put("numDataItems", 1)
                    => map:put("current", $nextDataItemGetter())
                    => map:put("endReached", false())
                    => map:put("getCurrent", %method fn() {?current})
                    => map:put("moveNext",  
                               %method fn() 
                                {
                                  let $nextDataItemGetter := $provider(?numDataItems)
                                    return
                                      if(not($nextDataItemGetter instance of function(*))) then gn:empty-generator($gen)
                                      else
                                        . => map:put("current", $nextDataItemGetter())
                                          => map:put("numDataItems", ?numDataItems + 1)
                                }
                               )
   return $nextGen                                                  
};   

declare function gn:make-generator-from-array($gen as f:generator, $input as array(*)) as f:generator
{
  let $size := array:size($input),
      $arrayProvider := fn($ind as xs:integer)
                        {
                          if($ind +1 gt $size) then -1
                           else fn(){$input($ind + 1)}
                        }
   return gn:make-generator($gen, $arrayProvider)
};  

declare function gn:make-generator-from-sequence($gen as f:generator, $input as item()*) as f:generator
{
  let $size := count($input),
      $seqProvider := fn($ind as xs:integer)
                        {
                          if($ind +1 gt $size) then -1
                           else fn(){$input[$ind + 1]}
                        }
   return gn:make-generator($gen, $seqProvider)
};   

declare function gn:make-generator-from-map($gen as f:generator, $inputMap as map(*)) as f:generator
        {
          let $keys := map:keys($inputMap),
              $size := map:size($inputMap),
              $mapProvider := fn($ind as xs:integer)
              {
                if($ind +1 gt $size) then -1
                  else fn() 
                       {
                         let $key := $keys[$ind + 1]
                          return
                            ( $key, [ $inputMap($key) ])
                       }
              }
            return
              $gen?make-generator($mapProvider)
        };

declare function gn:to-sequence($gen as f:generator) as item()* {gn:to-array($gen) => array:items()}; 

declare function gn:to-map($gen as f:generator) as map(*)
        {
          let $genPairs := $gen?for-each(fn($x)
                           {
                             let $key := head($x),
                                 $tail := tail($x),
                                 $value := if($tail instance of array(*)) 
                                           then for $ind in 1 to array:size($tail)
                                                 return $tail($ind)
                                           else $tail
                             return
                               map:pair($key, $value)
                           }
                         ) 
           return
             map:of-pairs($genPairs?to-sequence())
        };    

declare function gn:empty-generator($gen as f:generator) as f:generator 
{
  $gen => map:put("initialized", true()) => map:put("endReached", true())
    => map:put("getCurrent", %method fn() {error((),"getCurrent() called on an empty-generator")})
    => map:put("moveNext", %method fn() {error((),"moveNext() called on an empty-generator")})
};         

declare record f:generator 
   ( initialized as xs:boolean,
     endReached as xs:boolean,
     getCurrent as %method fn() as item()*,
     moveNext as %method fn(*) (: as f:generator, :),
     to-array := %method fn()
     {
        gn:to-array(.)
     },
     
     take := %method fn($n as xs:integer) as f:generator
     {
        gn:take(., $n)
     },
      
      take-while := %method fn($pred as function(item()*) as xs:boolean) as f:generator
      {
         gn:take-while(., $pred) 
      },
     
     skip-strict := %method fn($n as xs:integer, $issueErrorOnEmpty as xs:boolean) as f:generator
     {
        gn:skip-strict(., $n, $issueErrorOnEmpty)
     },
     skip := %method fn($n as xs:integer) as f:generator
     {
       gn:skip(., $n)
     },
     
     skip-while := %method fn($pred as function(item()*) as xs:boolean) as f:generator
     {
       gn:skip-while(., $pred)                 
     },
     
     some := %method fn() as xs:boolean
     {
       gn:some(.)
     },
     
     some-where := %method fn($pred) as xs:boolean
     {
       gn:some-where(., $pred)
     },
     
     first-where := %method fn($pred) as item()*
    {
     gn:first-where(., $pred)
    },
     
     subrange := %method fn($m as xs:integer, $n as xs:integer) as f:generator
     {
       gn:subrange(., $m, $n)
     },
     
     chunk := %method fn($size as xs:integer) as f:generator
     {
       gn:chunk(., $size)
     },
     
     head := %method fn() as item()* {gn:head(.)},
     tail := %method fn() as f:generator {gn:tail(.)},
     
     at := %method fn($ind) as item()* {gn:at(., $ind)},
     
     contains := %method fn($value as item()*) as xs:boolean
     {
       gn:contains(., $value)
     },
           
     for-each := %method fn($fun as function(*)) as f:generator
     {
       gn:for-each(., $fun)                 
      },
      
      for-each-pair := %method fn($gen2 as f:generator, $fun as function(*)) as f:generator
      {
        gn:for-each-pair(., $gen2, $fun)                    
      },
      
      zip := %method fn($gen2 as f:generator) as f:generator
      {
        gn:for-each-pair(., $gen2, fn($x1, $x2){[$x1, $x2]})
      },

      concat := %method fn($gen2 as f:generator) as f:generator
      {
        gn:concat(., $gen2)           
      },

      append := %method fn($value as item()*) as f:generator
      {
        gn:append(., $value)                    
      },
      
      prepend := %method fn($value as item()*) as f:generator
      {
        gn:prepend(., $value)
      },
      
      insert-at := %method fn($pos as xs:nonNegativeInteger, $value as item()*) as f:generator
      {
        gn:insert-at(., $pos, $value)               
      },
      
      remove-at := %method fn($pos as xs:nonNegativeInteger) as f:generator
      {
        gn:remove-at(., $pos)
      },
      
      remove-where := %method fn($predicate as function(item()*) as xs:boolean) as f:generator
      {
        gn:remove-where(., $predicate)
      },
      
      distinct := %method fn() as f:generator
      {
        gn:distinct(.)
      },   
      
      replace := %method fn($funIsMatching as function(item()*) as xs:boolean, $replacement as item()*) as f:generator
      {
        gn:replace(., $funIsMatching, $replacement)                  
      },
      
      reverse := %method fn() as f:generator
      {
        gn:reverse(.)
      },

      filter := %method fn($pred as function(item()*) as xs:boolean) as f:generator
      {
        gn:filter(., $pred)
      },   
      
      fold-left := %method fn($init as item()*, $action as fn(*)) as item()*
      {
        gn:fold-left(., $init, $action)
      },
        
      fold-right := %method fn($init as item()*, $action as fn(*)) as item()*
      {
        gn:fold-right(., $init, $action)
      },
      
      fold-lazy := %method fn($init as item()*, $action as fn(*), $shortCircuitProvider as function(*)) as item()*
      {
        gn:fold-lazy(., $init, $action, $shortCircuitProvider)
      },
      
      scan-left := %method fn($init as item()*, $action as fn(*)) as f:generator
      {
        gn:scan-left(., $init, $action)
      },

      scan-right := %method fn($init as item()*, $action as fn(*)) as f:generator
      {
        gn:scan-right(., $init, $action)
      },
        
      make-generator := %method fn($provider as function(*)) as f:generator
      {
        gn:make-generator(., $provider)                                             
      },
        
        make-generator-from-array := %method fn($input as array(*)) as f:generator
        {
          gn:make-generator-from-array(., $input)
        },
        
        make-generator-from-sequence := %method fn($input as item()*) as f:generator
        {
          gn:make-generator-from-sequence(., $input)
        },

        make-generator-from-map := %method fn($inputMap as map(*)) as f:generator
        {
          gn:make-generator-from-map(., $inputMap)
        },
        
        to-sequence := %method fn() as item()* {gn:to-sequence(.)},
        
        to-map := %method fn() as map(*) {gn:to-map(.)},       
        
        empty-generator := %method fn() as f:generator
        {
          gn:empty-generator(.)
        },      
     *
   );


let $gen2ToInf := f:generator(initialized := true(), endReached := false(), 
                              getCurrent := %method fn(){?last +1},
                              moveNext := %method fn()
                              {
                                if(not(?initialized))
                                  then map:put(., "initialized", true())
                                  else map:put(., "last", ?last + 1)
                              },
                              options := {"last" : 1}
                             ),
    $genN := $gen2ToInf?for-each(fn($n) {$n - 1}),
    $gen0toInf := $gen2ToInf?for-each(fn($n) {$n - 2}),
    $double := fn($n) {2*$n},
    $sum2 := fn($m, $n) {$m + $n},
    $product := fn($m, $n) {$m * $n},
    $factorial := fn($n) {fold-left(1 to $n, 1, $product)}
  return    
  (
    "$gen2ToInf?take(3)?to-array()",
    $gen2ToInf?take(3)?to-array(),
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
    "$gen2ToInf?subrange(4, 6)?to-array()",
    $gen2ToInf?subrange(4, 6)?to-array(),
    "$gen2ToInf?head()",
    $gen2ToInf?head(),
    "==>  $gen2ToInf?tail()?head()",
    $gen2ToInf?tail()?head(),
    "================", 
    "$gen2ToInf?subrange(4, 6)?tail()?to-array()",
    $gen2ToInf?subrange(4, 6)?tail()?to-array(),
    "================",
    "$gen2ToInf?at(5)",
    $gen2ToInf?at(5), 
    "================",
    "$gen2ToInf?subrange(1, 5)?to-array()",
    $gen2ToInf?subrange(1, 5)?to-array(),
    "$gen2ToInf?subrange(1, 5)?for-each($double)?to-array()",
    $gen2ToInf?subrange(1, 5)?for-each($double)?to-array(),
    "$gen2ToInf?take(5)?for-each($double)?to-array()",
    $gen2ToInf?take(5)?for-each($double)?to-array(),
    "==>  $gen2ToInf?for-each($double)?take(5)?to-array()",
    $gen2ToInf?for-each($double)?take(5)?to-array(),
    "================",
    "$gen2ToInf?subrange(1, 5)?to-array()",
    $gen2ToInf?subrange(1, 5)?to-array(),
    "$gen2ToInf?subrange(6, 10)?to-array()",
    $gen2ToInf?subrange(6, 10)?to-array(),
    "$gen2ToInf?subrange(1, 5)?for-each-pair($gen2ToInf?subrange(6, 10), $sum2)?to-array()",
    $gen2ToInf?subrange(1, 5)?for-each-pair($gen2ToInf?subrange(6, 10), $sum2)?to-array(), 
    "==>  $gen2ToInf?for-each-pair($gen2ToInf, $sum2)?take(5)?to-array()",
    $gen2ToInf?for-each-pair($gen2ToInf, $sum2)?take(5)?to-array(),
    "================",
    "==>  $gen2ToInf?filter(fn($n){$n mod 2 eq 1})?getCurrent()",
    $gen2ToInf?filter(fn($n){$n mod 2 eq 1})?getCurrent(),
    "$gen2ToInf?filter(fn($n){$n mod 2 eq 1})?moveNext()?getCurrent()",
    $gen2ToInf?filter(fn($n){$n mod 2 eq 1})?moveNext()?getCurrent(),
    "================", 
    "$gen2ToInf?filter(fn($n){$n mod 2 eq 1})?take(10)?to-array()",
    $gen2ToInf?filter(fn($n){$n mod 2 eq 1})?take(10)?to-array(),  
    "================", 
    "$gen2ToInf?filter(fn($n){$n mod 2 eq 1})?take(10)?to-sequence()",
    $gen2ToInf?filter(fn($n){$n mod 2 eq 1})?take(10)?to-sequence(),
    "================", 
    "$gen2ToInf?take-while(fn($n){$n lt 11})?to-array()",
    $gen2ToInf?take-while(fn($n){$n lt 11})?to-array(), 
    "$gen2ToInf?take-while(fn($n){$n lt 2})?to-array()",
    $gen2ToInf?take-while(fn($n){$n lt 2})?to-array(), 
    "================", 
    "$gen2ToInf?skip-while(fn($n){$n lt 11})?take(5)?to-array()",
    $gen2ToInf?skip-while(fn($n){$n lt 11})?take(5)?to-array(),
    "==> $gen2ToInf?skip-while(fn($n){$n lt 2})",
    $gen2ToInf?skip-while(fn($n){$n lt 2}),
    "
     ==> $gen2ToInf?skip-while(fn($n){$n lt 2})?skip(1)",
    $gen2ToInf?skip-while(fn($n){$n lt 2})?skip(1),
(:    $gen2ToInf?skip-while(fn($x) {$x ge 2}) :) (: ?skip(1) :) 
    "$gen2ToInf?some()",
     $gen2ToInf?some(),
     "let $empty := $gen2ToInf?empty-generator()
      return $empty?some()",
     let $empty := $gen2ToInf?empty-generator()
      return $empty?some(),
    "================",
    "$gen2ToInf?take(5)?filter(fn($n){$n ge 7})?some()",
     $gen2ToInf?take(5)?filter(fn($n){$n ge 7})?some(),  
     "$gen2ToInf?take(5)?some-where(fn($n){$n ge 7})",
     $gen2ToInf?take(5)?some-where(fn($n){$n ge 7}), 
     "$gen2ToInf?take(5)?some-where(fn($n){$n ge 6})",
     $gen2ToInf?take(5)?some-where(fn($n){$n ge 6}),
     "$gen2ToInf?some-where(fn($n){$n ge 100})",
     $gen2ToInf?some-where(fn($n){$n ge 100}),
     "================",
     "$gen2ToInf?take(10)?take(11)?to-array()",
     $gen2ToInf?take(10)?take(11)?to-array(),
     "$gen2ToInf?take(10)?skip(10)?to-array()",
     $gen2ToInf?take(10)?skip(10)?to-array(),
     "$gen2ToInf?take(10)?skip(9)?to-array()",     
     $gen2ToInf?take(10)?skip(9)?to-array(),
     "$gen2ToInf?take(10)?subrange(3, 12)?to-array()",
     $gen2ToInf?take(10)?subrange(3, 12)?to-array(),
     "$gen2ToInf?take(10)?subrange(5, 3)?to-array()",
     $gen2ToInf?take(10)?subrange(5, 3)?to-array(),
     "================",
     "$gen2ToInf?take(100)?chunk(20)?getCurrent()",
      $gen2ToInf?take(100)?chunk(20)?getCurrent(),
      "==>  $gen2ToInf?chunk(20)?take(5)?to-array()",
      $gen2ToInf?chunk(20)?take(5)?to-array(),
     "================",
     "$gen2ToInf?take(100)?chunk(20)?moveNext()?getCurrent()",
      $gen2ToInf?take(100)?chunk(20)?moveNext()?getCurrent(),
     "$gen2ToInf?take(100)?chunk(20)?moveNext()?moveNext()?getCurrent()", 
      $gen2ToInf?take(100)?chunk(20)?moveNext()?moveNext()?getCurrent(),
     "$gen2ToInf?take(100)?chunk(20)?skip(1)?getCurrent()",      
      $gen2ToInf?take(100)?chunk(20)?skip(1)?getCurrent(),
     "================",      
     "$gen2ToInf?take(100)?chunk(20)?for-each(fn($genX){$genX})?to-array()",      
      $gen2ToInf?take(100)?chunk(20)?for-each(fn($genX){$genX})?to-array(),
     "================",  
     "$gen2ToInf?take(10)?chunk(4)?to-array()",
      $gen2ToInf?take(10)?chunk(4)?to-array(),
      "$gen2ToInf?take(10)?chunk(4)?for-each(fn($arr){array:size($arr)})?to-array()",
      $gen2ToInf?take(10)?chunk(4)?for-each(fn($arr){array:size($arr)})?to-array(),
     "================", 
     "$gen2ToInf?subrange(10, 15)?concat($gen2ToInf?subrange(1, 9))?to-array()",
     $gen2ToInf?subrange(10, 15)?concat($gen2ToInf?subrange(1, 9))?to-array(),
     "================", 
     "$gen2ToInf?subrange(1, 5)?append(101)?to-array()",
     $gen2ToInf?subrange(1, 5)?append(101)?to-array(),
     "$gen2ToInf?subrange(1, 5)?prepend(101)?to-array()",
     $gen2ToInf?subrange(1, 5)?prepend(101)?to-array(),
     "==>  $gen2ToInf?append(101)",
     $gen2ToInf?append(101),
     "$gen2ToInf?prepend(101)?take(5)?to-array()",
     $gen2ToInf?prepend(101)?take(5)?to-array(),
     "================", 
     "$gen2ToInf?subrange(1, 5)?zip($gen2ToInf?subrange(6, 10))?to-array()",
     $gen2ToInf?subrange(1, 5)?zip($gen2ToInf?subrange(6, 10))?to-array(),
     "$gen2ToInf?subrange(1, 5)?zip($gen2ToInf?subrange(10, 20))?to-array()",
     $gen2ToInf?subrange(1, 5)?zip($gen2ToInf?subrange(10, 20))?to-array(),
     "==>  $gen2ToInf?zip($gen2ToInf?skip(5))?take(10)?to-array()",
     $gen2ToInf?zip($gen2ToInf?skip(5))?take(10)?to-array(),
     "$gen2ToInf?subrange(1, 5)?zip($gen2ToInf?subrange(10, 20))?zip($gen2ToInf?subrange(30, 40))?to-array()",
     $gen2ToInf?subrange(1, 5)?zip($gen2ToInf?subrange(10, 20))?zip($gen2ToInf?subrange(30, 40))?to-array(),
     "================", 
     "$gen2ToInf?make-generator(fn($numGenerated as xs:integer)
                                 {if($numGenerated le 9) then fn() {$numGenerated + 1} else -1} 
                             )?to-array()",
     $gen2ToInf?make-generator(fn($numGenerated as xs:integer)
                                 {if($numGenerated le 9) then fn() {$numGenerated + 1} else -1} 
                             )?to-array(),
     "================", 
     "$gen2ToInf?make-generator-from-array([1, 4, 9, 16, 25])?to-array()",
      $gen2ToInf?make-generator-from-array([1, 4, 9, 16, 25])?to-array(),
      "$gen2ToInf?make-generator-from-sequence((1, 8, 27, 64, 125))?to-array()",
      $gen2ToInf?make-generator-from-sequence((1, 8, 27, 64, 125))?to-array(), 
     "================", 
     "$gen2ToInf?take(10)?insert-at(3, ""XYZ"")?to-array()",
      $gen2ToInf?take(10)?insert-at(3, "XYZ")?to-array(),
      "$gen2ToInf?take(10)?insert-at(1, ""ABC"")?to-array()",
      $gen2ToInf?take(10)?insert-at(1, "ABC")?to-array(),
      "$gen2ToInf?take(10)?insert-at(11, ""PQR"")?to-array()",
      $gen2ToInf?take(10)?insert-at(11, "PQR")?to-array(),
      "==>  $gen2ToInf?insert-at(3, ""XYZ"")?take(10)?to-array()", 
      $gen2ToInf?insert-at(3, "XYZ")?take(10)?to-array(),
     (: , $gen2ToInf?take(10)?insert-at(12, "GHI")?to-array() :)  (:  Must raise error "Input Generator too-short." :) 
     "================", 
     "$gen2ToInf?take(10)?remove-at(3)?to-array()",
      $gen2ToInf?take(10)?remove-at(3)?to-array(),
      "$gen2ToInf?take(10)?remove-at(1)?to-array()",
      $gen2ToInf?take(10)?remove-at(1)?to-array(),
      "$gen2ToInf?take(10)?remove-at(10)?to-array()",
      $gen2ToInf?take(10)?remove-at(10)?to-array(),
      "==>  $gen2ToInf?remove-at(3)?take(10)?to-array()",
      $gen2ToInf?remove-at(3)?take(10)?to-array(),
      (: , $gen2ToInf?take(10)?remove-at(11)?to-array() :)        (:  Must raise error "Input Generator too-short." :) 
     "================",
     "==>  $gen2ToInf?remove-where(fn($x){$x mod 3 eq 0})?take(10)?to-array()",
      $gen2ToInf?remove-where(fn($x){$x mod 3 eq 0})?take(10)?to-array(),    
     "================",
     "$gen2ToInf?make-generator-from-sequence((1,  3, 1, 2,  1, 2, 5, 2, 5))?distinct()?to-array()",
      $gen2ToInf?make-generator-from-sequence((1,  3, 1, 2,  1, 2, 5, 2, 5))?distinct()?to-array(),
      "$gen2ToInf?for-each(fn($n){$n idiv 10})?take(50)?distinct()?to-array()",
      $gen2ToInf?for-each(fn($n){$n idiv 10})?take(50)?distinct()?to-array(),
      "$gen2ToInf?for-each(fn($n){$n idiv 10})?take(100)?distinct()?to-array()",
      $gen2ToInf?for-each(fn($n){$n idiv 10})?take(100)?distinct()?to-array(),
      "==> $gen2ToInf?for-each(fn($n){$n idiv 10})?distinct()?take(100)?to-array()",
      $gen2ToInf?for-each(fn($n){$n idiv 10})?distinct()?take(100)?to-array(),
     "================",          
     "$gen2ToInf?take(10)?replace(fn($x){$x gt 4}, ""Replacement"")?to-array()",
      $gen2ToInf?take(10)?replace(fn($x){$x gt 4}, "Replacement")?to-array(),
      "$gen2ToInf?take(10)?replace(fn($x){$x lt 3}, ""Replacement"")?to-array()",
      $gen2ToInf?take(10)?replace(fn($x){$x lt 3}, "Replacement")?to-array(),
      "$gen2ToInf?take(10)?replace(fn($x){$x gt 10}, ""Replacement"")?to-array()",
      $gen2ToInf?take(10)?replace(fn($x){$x gt 10}, "Replacement")?to-array(),
      "$gen2ToInf?take(10)?replace(fn($x){$x gt 11}, ""Replacement"")?to-array()",
      $gen2ToInf?take(10)?replace(fn($x){$x gt 11}, "Replacement")?to-array(),
      "$gen2ToInf?take(10)?replace(fn($x){$x lt 2}, ""Replacement"")?to-array()",
      $gen2ToInf?take(10)?replace(fn($x){$x lt 2}, "Replacement")?to-array(),
      "==> $gen2ToInf?replace(fn($x){$x gt 4}, ""Replacement"")?take(10)?to-array()",
      $gen2ToInf?replace(fn($x){$x gt 4}, "Replacement")?take(10)?to-array(),
      "$gen2ToInf?replace(fn($x){$x lt 3}, ""Replacement"")?take(10)?to-array()",
      $gen2ToInf?replace(fn($x){$x lt 3}, "Replacement")?take(10)?to-array(),
    (:  
      Will result in endless loop:
      
      , "==>  ==>  ==>  $gen2ToInf?replace(fn($x){$x lt 2}, ""Replacement"")?take(10)?to-array() <==  <==  <==",
      $gen2ToInf?replace2(fn($x){$x lt 2}, "Replacement")?take(10)?to-array() 
    :)
    "================",
    "$gen2ToInf?empty-generator()?reverse()?to-array()",
    $gen2ToInf?empty-generator()?reverse()?to-array(),
    "$gen2ToInf?empty-generator()?append(2)?reverse()?to-array()",
    $gen2ToInf?empty-generator()?append(2)?reverse()?to-array(),
    "$gen2ToInf?take(10)?reverse()?to-array()",
    $gen2ToInf?take(10)?reverse()?to-array(),
    "================",
    "$genN?take(10)?contains(3)",
    $genN?take(10)?contains(3),
    "$genN?take(10)?contains(20)",
    $genN?take(10)?contains(20),
    "$genN?take(10)?contains(1)",    
    $genN?take(10)?contains(1), 
    "$genN?take(10)?contains(10)",     
    $genN?take(10)?contains(10),  
    "$genN?take(10)?contains(0)",
    $genN?take(10)?contains(0), 
    "$genN?take(10)?contains(11)",        
    $genN?take(10)?contains(11),
    "==> $genN?contains(15)",    
    $genN?contains(15), 
    "================",
    "$gen2ToInf?take(5)?fold-left(0, fn($x, $y){$x + $y})",
    $gen2ToInf?take(5)?fold-left(0, fn($x, $y){$x + $y}),
    "================",
    "$gen2ToInf?take(5)?fold-right(0, fn($x, $y){$x + $y})",
    $gen2ToInf?take(5)?fold-right(0, fn($x, $y){$x + $y}),
    "================",
    "==> $gen0toInf?for-each(fn($n){(2 * $n + 1) div $factorial(2*xs:decimal($n)})
             ?take(8)?fold-left(0, fn($x, $y){$x + $y})",
    $gen0toInf?for-each(fn($n){(2*$n + 1) div $factorial(2*xs:decimal($n))})?take(8)?fold-left(0, fn($x, $y){$x + $y}),
    "$gen0toInf?for-each(fn($n){(2*$n + 1) div $factorial(2*xs:decimal($n))})?take(8)?scan-left(0, fn($x, $y){$x + $y})?to-array()",
    $gen0toInf?for-each(fn($n){(2*$n + 1) div $factorial(2*xs:decimal($n))})?take(8)?scan-left(0, fn($x, $y){$x + $y})?to-array(),
    "let $genSeqE := $gen0toInf?for-each(fn($n){(2*$n + 1) div $factorial(2*xs:decimal($n))})?take(8)?scan-left(0, fn($x, $y){$x + $y}),
    $genSeqE-Next := $genSeqE?tail(),
    $genZipped := $genSeqE?zip($genSeqE-Next)
 return
    $genZipped?first-where(fn($pair){abs($pair(1) - $pair(2)) lt 0.000001})(2)",
    let $genSeqE := $gen0toInf?for-each(fn($n){(2*$n + 1) div $factorial(2*xs:decimal($n))})?take(8)?scan-left(0, fn($x, $y){$x + $y}),
        $genSeqE-Next := $genSeqE?tail(),
        $genZipped := $genSeqE?zip($genSeqE-Next)
      return
        $genZipped?first-where(fn($pair){abs($pair(1) - $pair(2)) lt 0.000001})(2), 
    "================",
    
    "$gen2ToInf?empty-generator()?scan-left(0, fn($x, $y){$x + $y})?to-array()",
    $gen2ToInf?empty-generator()?scan-left(0, fn($x, $y){$x + $y})?to-array(),
    "$gen2ToInf?take(5)?scan-left(0, fn($x, $y){$x + $y})?to-array()",
    $gen2ToInf?take(5)?scan-left(0, fn($x, $y){$x + $y})?to-array(),
    "================",
    "$gen2ToInf?make-generator-from-sequence((1 to 10))?scan-right(0, fn($x, $y){$x + $y})?to-array()",
    $gen2ToInf?make-generator-from-sequence((1 to 10))?scan-right(0, fn($x, $y){$x + $y})?to-array(),
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
     ),
     "===============",
     "     let $myMap := {'John': 22, 'Ann': 28, 'Peter': 31}
      return 
        $gen2ToInf?make-generator-from-map($myMap)?to-array()",
     let $myMap := {"John": 22, "Ann": 28, "Peter": 31}
      return 
        $gen2ToInf?make-generator-from-map($myMap)?to-array(),
     "===============",        
     "let $myMap := {'John': 22, 'Ann': 28, 'Peter': 31},
          $genMap := $gen2ToInf?make-generator-from-map($myMap)
      return
        $genMap?to-map()" ,
     let $myMap := {"John": 22, "Ann": 28, "Peter": 31},
         $genMap := $gen2ToInf?make-generator-from-map($myMap)
      return
        $genMap?to-map(),
     "$gen2ToInf?take(10)?to-map()",
     $gen2ToInf?take(10)?to-map()     
   )
