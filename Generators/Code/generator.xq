declare namespace f = "http://www.w3.org/2005/xpath-functions-2025";
declare namespace gn = "http://www.w3.org/2005/xpath-functions-2025/generator";
declare function gn:to-array($gen as f:generator) as array(*)
{
   while-do( [$gen, []],
          function( $inArr) 
          { $inArr(1)?initialized and not($inArr(1)?end-reached) },                 
          function($inArr) 
          { array{$inArr(1) =?> move-next(), 
                  array:append($inArr(2), $inArr(1) =?> get-current())
                 } 
           }         
 ) (2)
};

declare function gn:take($gen as f:generator, $n as xs:integer) as f:generator
{
  let $gen := if(not($gen?initialized)) then $gen =?> move-next()
                else $gen
   return
     if($gen?end-reached or $n le 0) then gn:empty-generator($gen)
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
     if($gen?end-reached) then gn:empty-generator($gen)
      else      
        let $current := $gen =?> get-current()
          return
            if(not($pred($current))) then gn:empty-generator($gen)
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
                 else gn:empty-generator($gen)
    else 
      let $gen := if(not($gen?initialized)) then $gen =?> move-next()
                   else $gen
        return
          if(not($gen?end-reached)) then gn:skip-strict($gen =?> move-next(), $n -1, $issueErrorOnEmpty)
            else gn:empty-generator($gen)    
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
     if($gen?end-reached) then gn:empty-generator($gen)
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

declare function gn:first-where($gen as f:generator, $pred) as item()*
{
 gn:head(gn:filter($gen, $pred))
};

declare function gn:chunk($gen as f:generator, $size as xs:positiveInteger) as f:generator
{
  let $gen := if(not($gen?initialized)) then $gen =?> move-next()
                else $gen
   return
     if($gen?end-reached) then gn:empty-generator($gen)
     else
       let $thisChunk := gn:to-array(gn:take($gen, $size)),
           $cutGen := gn:skip($gen, $size),
           $resultGen := $gen => map:put("get-current",   fn($this as f:generator){$thisChunk})
                              => map:put("move-next",   fn($this as f:generator){gn:chunk($cutGen, $size)})
        return $resultGen  
};

declare function gn:head($gen as f:generator) as item()* {gn:take($gen, 1) =?> get-current()};

declare function gn:tail($gen as f:generator) as f:generator {gn:skip($gen, 1)};

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
     if($gen?end-reached) then gn:empty-generator($gen)
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
      if($gen?end-reached or $gen2?end-reached) then gn:empty-generator($gen) 
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
                               => map:put("move-next",   fn($this as f:generator){gn:empty-generator($gen)})
                               => map:put("end-reached", false())
         return
           gn:concat($gen, $genSingle)                    
      };  
      
declare function gn:prepend($gen as f:generator, $value as item()*) as f:generator
      {
        let $gen := if(not($gen?initialized)) then $gen =?> move-next()
            else $gen,
            $genSingle := $gen => map:put("get-current",   fn($this as f:generator){$value})
                               => map:put("move-next",   fn($this as f:generator){gn:empty-generator($gen)})
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
                                             let $intendedReplace := function($z) {$z =?> replace($funIsMatching, $replacement)}
                                              return
                                                if($nextGen?end-reached) then $nextGen
                                                else $intendedReplace($nextGen)
                                           }
                                        )
      };      
      
declare function gn:reverse($gen as f:generator) as f:generator
{
  if($gen?end-reached) then gn:empty-generator($gen)
    else
     let $current := $gen =?> get-current()
       return
         gn:append(gn:reverse(gn:tail($gen)), $current)
};      
      
declare function gn:filter($gen as f:generator, $pred as function(item()*) as xs:boolean) as f:generator
{
 if($gen?initialized and $gen?end-reached) then gn:empty-generator($gen)
  else
    let $getNextGoodGen := function($gen as map(*), 
                                 $pred as function(item()*) as xs:boolean)
       {
          if($gen?end-reached) then gn:empty-generator($gen)
          else
            let $mapResult := 
                  while-do(
                           $gen,
                           function($x) { not($x?end-reached) and not($pred($x =?> get-current()))},
                           function($x) { $x =?> move-next() }
                           )   
            return 
              if($mapResult?end-reached) then gn:empty-generator($gen)
               else $mapResult                  
       },
       
       $gen := if($gen?initialized) then $gen 
                 else $gen =?> move-next(),
       $nextGoodGen := $getNextGoodGen($gen, $pred)
    return
      if($nextGoodGen?end-reached) then gn:empty-generator($gen)
      else
        $nextGoodGen  => map:put("inputGen", $nextGoodGen)
                     => map:put("move-next", 
                                  fn($this as f:generator) 
                                  {
                                    let $nextGoodGen := $getNextGoodGen($this?inputGen =?> move-next(), $pred)
                                      return
                                        if($nextGoodGen?end-reached) then gn:empty-generator($nextGoodGen)
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

declare function gn:fold-right($gen as f:generator, $init as item()*, $action as fn(*)) as item()*
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
  let $resultGen := $gen =?> empty-generator() 
                        => map:put("end-reached", false())
                        => map:put("get-current",   fn($this as f:generator){$init})
   return
     if($gen?end-reached) 
       then $resultGen => map:put("move-next",   fn($this as f:generator){$gen =?> empty-generator()})
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

declare function gn:make-generator($gen as f:generator, $provider as function(*)) as f:generator
{
 let $gen := if(not($gen?initialized)) then $gen =?> move-next()
            else $gen,
      $nextDataItemGetter := $provider(0),      
      $nextGen := if(not($nextDataItemGetter instance of function(*))) 
                   then gn:empty-generator($gen)  
                   else gn:empty-generator($gen)
                    => map:put("numDataItems", 1)
                    => map:put("current", $nextDataItemGetter())
                    => map:put("end-reached", false())
                    => map:put("get-current", fn($this as f:generator) {$this?current})
                    => map:put("move-next",  
                                 fn($this as f:generator) 
                                {
                                  let $nextDataItemGetter := $provider($this?numDataItems)
                                    return
                                      if(not($nextDataItemGetter instance of function(*))) then gn:empty-generator($gen)
                                      else
                                        $this => map:put("current", $nextDataItemGetter())
                                          => map:put("numDataItems", $this?numDataItems + 1)
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
              $gen =?> make-generator($mapProvider)
        };

declare function gn:to-sequence($gen as f:generator) as item()* {gn:to-array($gen) => array:items()}; 

declare function gn:to-map($gen as f:generator) as map(*)
        {
          let $genPairs := $gen =?> for-each(fn($x)
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
             map:of-pairs($genPairs =?> to-sequence())
        };    

declare function gn:empty-generator($gen as f:generator) as f:generator 
{
  $gen => map:put("initialized", true()) => map:put("end-reached", true())
    => map:put("get-current",   fn($this as f:generator) {error((),"get-current() called on an empty-generator")})
    => map:put("move-next",   fn($this as f:generator) {error((),"move-next()) called on an empty-generator")})
};         

declare record f:generator 
   ( initialized as xs:boolean,
     end-reached as xs:boolean,
     get-current as   fn($this as f:generator) as item()*,
     move-next as   fn($this as f:generator) as f:generator,  (: as item()*, :)
     to-array :=   fn($this as f:generator)
     {
        gn:to-array($this)
     },
     
     take :=   fn($gen as f:generator, $n as xs:integer) as f:generator
     {
        gn:take($gen, $n)
     },
      
      take-while :=   fn($this as f:generator, $pred as function(item()*) as xs:boolean) as f:generator
      {
         gn:take-while($this, $pred) 
      },
     
     skip-strict :=   fn($n as xs:integer, $issueErrorOnEmpty as xs:boolean) as f:generator
     {
        gn:skip-strict(., $n, $issueErrorOnEmpty)
     },
     skip :=   fn($this as f:generator, $n as xs:integer) as f:generator
     {
       gn:skip($this, $n)
     },
     
     skip-while :=   fn($this as f:generator, $pred as function(item()*) as xs:boolean) as f:generator
     {
       gn:skip-while($this, $pred)                 
     },
     
     some :=   fn($this as f:generator) as xs:boolean
     {
       gn:some($this)
     },
     
     some-where :=   fn($this as f:generator, $pred) as xs:boolean
     {
       gn:some-where($this, $pred)
     },
     
     first-where :=   fn($this as f:generator, $pred) as item()*
    {
     gn:first-where($this, $pred)
    },
     
     subrange :=   fn($this as f:generator, $m as xs:integer, $n as xs:integer) as f:generator
     {
       gn:subrange($this, $m, $n)
     },
     
     chunk :=   fn($this as f:generator, $size as xs:integer) as f:generator
     {
       gn:chunk($this, $size)
     },
     
     head :=   fn($this as f:generator) as item()* {gn:head($this)},
     tail :=   fn($this as f:generator) as f:generator {gn:tail($this)},
     
     at :=   fn($this as f:generator, $ind) as item()* {gn:at($this, $ind)},
     
     contains :=   fn($this as f:generator,$value as item()*) as xs:boolean
     {
       gn:contains($this, $value)
     },
           
     for-each :=   fn($this as f:generator, $fun as function(*)) as f:generator
     {
       gn:for-each($this, $fun)                 
      },
           
      for-each-pair :=   fn($this as f:generator, $gen2 as f:generator, $fun as function(*)) as f:generator
      {
        gn:for-each-pair($this, $gen2, $fun)                    
      },
      
      zip :=   fn($this as f:generator, $gen2 as f:generator) as f:generator
      {
        gn:for-each-pair($this, $gen2, fn($x1, $x2){[$x1, $x2]})
      },

      concat :=   fn($this as f:generator, $gen2 as f:generator) as f:generator
      {
        gn:concat($this, $gen2)           
      },

      append :=   fn($this as f:generator, $value as item()*) as f:generator
      {
        gn:append($this, $value)                    
      },
      
      prepend :=   fn($this as f:generator, $value as item()*) as f:generator
      {
        gn:prepend($this, $value)
      },
      
      insert-at :=   fn($this as f:generator, $pos as xs:nonNegativeInteger, $value as item()*) as f:generator
      {
        gn:insert-at($this, $pos, $value)               
      },
      
      remove-at :=   fn($this as f:generator, $pos as xs:nonNegativeInteger) as f:generator
      {
        gn:remove-at($this, $pos)
      },
      
      remove-where :=   fn($this as f:generator, $predicate as function(item()*) as xs:boolean) as f:generator
      {
        gn:remove-where($this, $predicate)
      },
      
      distinct :=   fn($this as f:generator) as f:generator
      {
        gn:distinct($this)
      },   
      
      replace :=   fn($this as f:generator, $funIsMatching as function(item()*) as xs:boolean, $replacement as item()*) as f:generator
      {
        gn:replace($this, $funIsMatching, $replacement)                  
      },
      
      reverse :=   fn($this as f:generator) as f:generator
      {
        gn:reverse($this)
      },

      filter :=   fn($this as f:generator, $pred as function(item()*) as xs:boolean) as f:generator
      {
        gn:filter($this, $pred)
      },   
      
      fold-left :=   fn($this as f:generator, $init as item()*, $action as fn(*)) as item()*
      {
        gn:fold-left($this, $init, $action)
      },
        
      fold-right :=   fn($this as f:generator, $init as item()*, $action as fn(*)) as item()*
      {
        gn:fold-right($this, $init, $action)
      },
      
      fold-lazy :=   fn($this as f:generator, $init as item()*, $action as fn(*), $shortCircuitProvider as function(*)) as item()*
      {
        gn:fold-lazy($this, $init, $action, $shortCircuitProvider)
      },
      
      scan-left :=   fn($this as f:generator, $init as item()*, $action as fn(*)) as f:generator
      {
        gn:scan-left($this, $init, $action)
      },

      scan-right :=   fn($this as f:generator, $init as item()*, $action as fn(*)) as f:generator
      {
        gn:scan-right($this, $init, $action)
      },
        
      make-generator :=   fn($this as f:generator, $provider as function(*)) as f:generator
      {
        gn:make-generator($this, $provider)                                             
      },
        
        make-generator-from-array :=   fn($this as f:generator, $input as array(*)) as f:generator
        {
          gn:make-generator-from-array($this, $input)
        },
        
        make-generator-from-sequence :=   fn($this as f:generator, $input as item()*) as f:generator
        {
          gn:make-generator-from-sequence($this, $input)
        },

        make-generator-from-map := fn($this as f:generator, $inputMap as map(*)) as f:generator
        {
          gn:make-generator-from-map($this, $inputMap)
        },
        
        to-sequence :=   fn($this as f:generator) as item()* {gn:to-sequence($this)},
        
        to-map :=   fn($this as f:generator) as map(*) {gn:to-map($this)},       
        
        empty-generator :=   fn($this as f:generator) as f:generator
        {
          gn:empty-generator($this)
        },      
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
    $genN := $gen2ToInf =?> for-each(fn($n) {$n - 1}),
    $gen0toInf := $gen2ToInf =?> for-each(fn($n) {$n - 2}),
    $double := fn($n) {2*$n},
    $sum2 := fn($m, $n) {$m + $n},
    $product := fn($m, $n) {$m * $n},
    $factorial := fn($n) {fold-left(1 to $n, 1, $product)}
  return    
  (
    "$gen2ToInf =?>take(3) =?> to-array()",
    $gen2ToInf =?>take(3) =?> to-array(),
    "================",    
    "$gen2ToInf =?>take(3) =?> skip(2) =?> get-current()",
    $gen2ToInf =?> take(3) =?> skip(2) =?> get-current(),
    (: $gen2ToInf =?>take(3) =?> move-next() =?> move-next() =?> move-next() =?> get-current(), :)
    "================",
    "$gen2ToInf =?> get-current()",
    $gen2ToInf =?> get-current(),
    "$gen2ToInf =?> move-next() =?> get-current()",
    $gen2ToInf =?> move-next()  =?> get-current(),
    "================",
    "$gen2ToInf =?>take(5) instance of f:generator",
    $gen2ToInf =?> take(5) instance of f:generator,
    "==>  $gen2ToInf =?> skip(7) instance of f:generator",
    $gen2ToInf =?> skip(7) instance of f:generator,  
    "================",
    "$gen2ToInf =?> subrange(4, 6) =?> get-current()",
    $gen2ToInf =?> subrange(4, 6) =?> get-current(), 
    "$gen2ToInf =?> subrange(4, 6) =?> move-next() =?> get-current()",
    $gen2ToInf =?> subrange(4, 6) =?> move-next() =?> get-current(),
    "$gen2ToInf =?> subrange(4, 6) =?> move-next() =?> move-next() =?> get-current()",
    $gen2ToInf =?> subrange(4, 6) =?> move-next() =?> move-next() =?> get-current(),
    (: $gen2ToInf =?> subrange(4, 6) =?> move-next() =?> move-next() =?> move-next() =?> get-current() :) (: Must raise error:)    
    "================",    
    "$gen2ToInf =?> subrange(4, 6) =?> head()",
    $gen2ToInf =?> subrange(4, 6) =?> head(),  
    "$gen2ToInf =?> subrange(4, 6) =?> tail() =?> head()",
    $gen2ToInf =?> subrange(4, 6) =?> tail() =?> head(),
    "$gen2ToInf =?> subrange(4, 6) =?> to-array()",
    $gen2ToInf =?> subrange(4, 6) =?> to-array(),
    "$gen2ToInf =?> head()",
    $gen2ToInf =?> head(),
    "==>  $gen2ToInf =?> tail() =?> head()",
    $gen2ToInf =?> tail() =?> head(),
    "================", 
    "$gen2ToInf =?> subrange(4, 6) =?> tail() =?> to-array()",
    $gen2ToInf =?> subrange(4, 6) =?> tail() =?> to-array(),
    "================",
    "$gen2ToInf =?> at(5)",
    $gen2ToInf =?> at(5), 
    "================",
    "$gen2ToInf =?> subrange(1, 5) =?> to-array()",
    $gen2ToInf =?> subrange(1, 5) =?> to-array(),
    "$gen2ToInf =?> subrange(1, 5) =?> for-each($double) =?> to-array()",
    $gen2ToInf =?> subrange(1, 5) =?> for-each($double) =?> to-array(),
    "$gen2ToInf =?>take(5) =?> for-each($double) =?> to-array()",
    $gen2ToInf =?>take(5) =?> for-each($double) =?> to-array(),
    "==>  $gen2ToInf =?> for-each($double) =?> take(5) =?> to-array()",
    $gen2ToInf =?> for-each($double) =?> take(5) =?> to-array(),
    "================",
    "$gen2ToInf =?> subrange(1, 5) =?> to-array()",
    $gen2ToInf =?> subrange(1, 5) =?> to-array(),
    "$gen2ToInf =?> subrange(6, 10) =?> to-array()",
    $gen2ToInf =?> subrange(6, 10) =?> to-array(),
    "$gen2ToInf =?> subrange(1, 5) =?> for-each-pair($gen2ToInf =?> subrange(6, 10), $sum2) =?> to-array()",
    $gen2ToInf =?> subrange(1, 5) =?> for-each-pair($gen2ToInf =?> subrange(6, 10), $sum2) =?> to-array(), 
    "==>  $gen2ToInf =?> for-each-pair($gen2ToInf, $sum2) =?> take(5) =?> to-array()",
    $gen2ToInf =?> for-each-pair($gen2ToInf, $sum2) =?> take(5) =?> to-array(),
    "================",
    "==>  $gen2ToInf =?> filter(fn($n){$n mod 2 eq 1}) =?> get-current()",
    $gen2ToInf =?> filter(fn($n){$n mod 2 eq 1}) =?> get-current(),

    "$gen2ToInf =?> filter(fn($n){$n mod 2 eq 1}) =?> move-next() =?> get-current()",
    $gen2ToInf =?> filter(fn($n){$n mod 2 eq 1}) =?> move-next() =?> get-current(),
    "================", 
    "$gen2ToInf =?> filter(fn($n){$n mod 2 eq 1}) =?> take(10) =?> to-array()",
    $gen2ToInf =?> filter(fn($n){$n mod 2 eq 1}) =?> take(10) =?> to-array(),  
    "================", 
    "$gen2ToInf =?> filter(fn($n){$n mod 2 eq 1}) =?> take(10) =?> to-sequence()",
    $gen2ToInf =?> filter(fn($n){$n mod 2 eq 1}) =?> take(10) =?> to-sequence(),

    "================", 
    "$gen2ToInf =?> take-while(fn($n){$n lt 11}) =?> to-array()",
    $gen2ToInf =?> take-while(fn($n){$n lt 11}) =?> to-array(), 
    "$gen2ToInf =?> take-while(fn($n){$n lt 2}) =?> to-array()",
    $gen2ToInf =?> take-while(fn($n){$n lt 2}) =?> to-array(), 
    "================", 
    "$gen2ToInf =?> skip-while(fn($n){$n lt 11}) =?> take(5) =?> to-array()",
    $gen2ToInf =?> skip-while(fn($n){$n lt 11}) =?> take(5) =?> to-array(),
    "==> $gen2ToInf =?> skip-while(fn($n){$n lt 2})",
    $gen2ToInf =?> skip-while(fn($n){$n lt 2}),
    "
     ==> $gen2ToInf =?> skip-while(fn($n){$n lt 2}) =?> skip(1)",
     $gen2ToInf =?> skip-while(fn($n){$n lt 2}) =?> skip(1),
    "$gen2ToInf =?> some()",
     $gen2ToInf =?> some(),
     "let $empty := $gen2ToInf =?> empty-generator()
      return $empty =?> some()",
     let $empty := $gen2ToInf =?> empty-generator()
      return $empty =?> some(),
    "================",
    "$gen2ToInf =?>take(5) =?> filter(fn($n){$n ge 7}) =?> some()",
     $gen2ToInf =?>take(5) =?> filter(fn($n){$n ge 7}) =?> some(),  
     "$gen2ToInf =?>take(5) =?> some-where(fn($n){$n ge 7})",
     $gen2ToInf =?>take(5) =?> some-where(fn($n){$n ge 7}), 
     "$gen2ToInf =?>take(5) =?> some-where(fn($n){$n ge 6})",
     $gen2ToInf =?>take(5) =?> some-where(fn($n){$n ge 6}),
     "$gen2ToInf =?> some-where(fn($n){$n ge 100})",
     $gen2ToInf =?> some-where(fn($n){$n ge 100}),
     "================",
     "$gen2ToInf =?>take(10) =?> take(11) =?> to-array()",
     $gen2ToInf =?>take(10) =?> take(11) =?> to-array(),
     "$gen2ToInf =?>take(10) =?> skip(10) =?> to-array()",
     $gen2ToInf =?>take(10) =?> skip(10) =?> to-array(),
     "$gen2ToInf =?>take(10) =?> skip(9) =?> to-array()",     
     $gen2ToInf =?>take(10) =?> skip(9) =?> to-array(),
     "$gen2ToInf =?>take(10) =?> subrange(3, 12) =?> to-array()",
     $gen2ToInf =?>take(10) =?> subrange(3, 12) =?> to-array(),
     "$gen2ToInf =?>take(10) =?> subrange(5, 3) =?> to-array()",
     $gen2ToInf =?>take(10) =?> subrange(5, 3) =?> to-array(),
     "================",
     "$gen2ToInf =?>take(100) =?> chunk(20) =?> get-current()",
      $gen2ToInf =?>take(100) =?> chunk(20) =?> get-current(),
      "==>  $gen2ToInf =?> chunk(20) =?> take(5) =?> to-array()",
      $gen2ToInf =?> chunk(20) =?> take(5) =?> to-array(),
     "================",
     "$gen2ToInf =?>take(100) =?> chunk(20) =?> move-next() =?> get-current()",
      $gen2ToInf =?>take(100) =?> chunk(20) =?> move-next() =?> get-current(),
     "$gen2ToInf =?>take(100) =?> chunk(20) =?> move-next() =?> move-next() =?> get-current()", 
      $gen2ToInf =?>take(100) =?> chunk(20) =?> move-next() =?> move-next() =?> get-current(),
     "$gen2ToInf =?>take(100) =?> chunk(20) =?> skip(1) =?> get-current()",      
      $gen2ToInf =?>take(100) =?> chunk(20) =?> skip(1) =?> get-current(),
     "================",      
     "$gen2ToInf =?>take(100) =?> chunk(20) =?> for-each(fn($genX){$genX}) =?> to-array()",      
      $gen2ToInf =?>take(100) =?> chunk(20) =?> for-each(fn($genX){$genX}) =?> to-array(),
     "================",  
     "$gen2ToInf =?>take(10) =?> chunk(4) =?> to-array()",
      $gen2ToInf =?>take(10) =?> chunk(4) =?> to-array(),
      "$gen2ToInf =?>take(10) =?> chunk(4) =?> for-each(fn($arr){array:size($arr)}) =?> to-array()",
      $gen2ToInf =?>take(10) =?> chunk(4) =?> for-each(fn($arr){array:size($arr)}) =?> to-array(),
     "================", 
     "$gen2ToInf =?> subrange(10, 15) =?> concat($gen2ToInf =?> subrange(1, 9)) =?> to-array()",
     $gen2ToInf =?> subrange(10, 15) =?> concat($gen2ToInf =?> subrange(1, 9)) =?> to-array(),
     "================", 
     "$gen2ToInf =?> subrange(1, 5) =?> append(101) =?> to-array()",
     $gen2ToInf =?> subrange(1, 5) =?> append(101) =?> to-array(),
     "$gen2ToInf =?> subrange(1, 5) =?> prepend(101) =?> to-array()",
     $gen2ToInf =?> subrange(1, 5) =?> prepend(101) =?> to-array(),
     "==>  $gen2ToInf =?> append(101)",
     $gen2ToInf =?> append(101),
     "$gen2ToInf =?> prepend(101) =?> take(5) =?> to-array()",
     $gen2ToInf =?> prepend(101) =?> take(5) =?> to-array(),
     "================", 
     "$gen2ToInf =?> subrange(1, 5) =?> zip($gen2ToInf =?> subrange(6, 10)) =?> to-array()",
     $gen2ToInf =?> subrange(1, 5) =?> zip($gen2ToInf =?> subrange(6, 10)) =?> to-array(),
     "$gen2ToInf =?> subrange(1, 5) =?> zip($gen2ToInf =?> subrange(10, 20)) =?> to-array()",
     $gen2ToInf =?> subrange(1, 5) =?> zip($gen2ToInf =?> subrange(10, 20)) =?> to-array(),
     "==>  $gen2ToInf =?> zip($gen2ToInf =?> skip(5)) =?> take(10) =?> to-array()",
     $gen2ToInf =?> zip($gen2ToInf =?> skip(5)) =?> take(10) =?> to-array(),
     "$gen2ToInf =?> subrange(1, 5) =?> zip($gen2ToInf =?> subrange(10, 20)) =?> zip($gen2ToInf =?> subrange(30, 40)) =?> to-array()",
     $gen2ToInf =?> subrange(1, 5) =?> zip($gen2ToInf =?> subrange(10, 20)) =?> zip($gen2ToInf =?> subrange(30, 40)) =?> to-array(),
     "================", 
     "$gen2ToInf =?> make-generator(fn($numGenerated as xs:integer)
                                 {if($numGenerated le 9) then fn() {$numGenerated + 1} else -1} 
                             ) =?> to-array()",
     $gen2ToInf =?> make-generator(fn($numGenerated as xs:integer)
                                 {if($numGenerated le 9) then fn() {$numGenerated + 1} else -1} 
                             ) =?> to-array(),
     "================", 
     "$gen2ToInf =?> make-generator-from-array([1, 4, 9, 16, 25]) =?> to-array()",
      $gen2ToInf =?> make-generator-from-array([1, 4, 9, 16, 25]) =?> to-array(),
      "$gen2ToInf =?> make-generator-from-sequence((1, 8, 27, 64, 125)) =?> to-array()",
      $gen2ToInf =?> make-generator-from-sequence((1, 8, 27, 64, 125)) =?> to-array(), 
     "================", 
     "$gen2ToInf =?>take(10) =?> insert-at(3, ""XYZ"") =?> to-array()",
      $gen2ToInf =?>take(10) =?> insert-at(3, "XYZ") =?> to-array(),
      "$gen2ToInf =?>take(10) =?> insert-at(1, ""ABC"") =?> to-array()",
      $gen2ToInf =?>take(10) =?> insert-at(1, "ABC") =?> to-array(),
      "$gen2ToInf =?>take(10) =?> insert-at(11, ""PQR"") =?> to-array()",
      $gen2ToInf =?>take(10) =?> insert-at(11, "PQR") =?> to-array(),
      "==>  $gen2ToInf =?> insert-at(3, ""XYZ"") =?> take(10) =?> to-array()", 
      $gen2ToInf =?> insert-at(3, "XYZ") =?> take(10) =?> to-array(),
     (: , $gen2ToInf =?>take(10) =?> insert-at(12, "GHI") =?> to-array() :)  (:  Must raise error "Input Generator too-short." :) 
     "================", 
     "$gen2ToInf =?>take(10) =?> remove-at(3) =?> to-array()",
      $gen2ToInf =?>take(10) =?> remove-at(3) =?> to-array(),
      "$gen2ToInf =?>take(10) =?> remove-at(1) =?> to-array()",
      $gen2ToInf =?>take(10) =?> remove-at(1) =?> to-array(),
      "$gen2ToInf =?>take(10) =?> remove-at(10) =?> to-array()",
      $gen2ToInf =?>take(10) =?> remove-at(10) =?> to-array(),
      "==>  $gen2ToInf =?> remove-at(3) =?> take(10) =?> to-array()",
      $gen2ToInf =?> remove-at(3) =?> take(10) =?> to-array(),
      (: , $gen2ToInf =?>take(10) =?> remove-at(11) =?> to-array() :)        (:  Must raise error "Input Generator too-short." :) 
(::) 
     "================",
     "==>  $gen2ToInf =?> remove-where(fn($x){$x mod 3 eq 0}) =?> take(10) =?> to-array()",
      $gen2ToInf =?> remove-where(fn($x){$x mod 3 eq 0}) =?> take(10) =?> to-array(),   
       
     "================",
     "$gen2ToInf =?> make-generator-from-sequence((1,  3, 1, 2,  1, 2, 5, 2, 5)) =?> distinct() =?> to-array()",
      $gen2ToInf =?> make-generator-from-sequence((1,  3, 1, 2,  1, 2, 5, 2, 5)) =?> distinct() =?> to-array(),
      "$gen2ToInf =?> for-each(fn($n){$n idiv 10}) =?> take(50) =?> distinct() =?> to-array()",
      $gen2ToInf =?> for-each(fn($n){$n idiv 10}) =?> take(50) =?> distinct() =?> to-array(),
      "$gen2ToInf =?> for-each(fn($n){$n idiv 10}) =?> take(100) =?> distinct() =?> to-array()",
      $gen2ToInf =?> for-each(fn($n){$n idiv 10}) =?> take(100) =?> distinct() =?> to-array(),
      "==> $gen2ToInf =?> for-each(fn($n){$n idiv 10}) =?> distinct() =?> take(35) =?> to-array()",
      $gen2ToInf =?> for-each(fn($n){$n idiv 10}) =?> distinct() =?> take(35) =?> to-array(),
     
     "================",          
     "$gen2ToInf =?>take(10) =?> replace(fn($x){$x gt 4}, ""Replacement"") =?> to-array()",
      $gen2ToInf =?>take(10) =?> replace(fn($x){$x gt 4}, "Replacement") =?> to-array(),
      "$gen2ToInf =?>take(10) =?> replace(fn($x){$x lt 3}, ""Replacement"") =?> to-array()",
      $gen2ToInf =?>take(10) =?> replace(fn($x){$x lt 3}, "Replacement") =?> to-array(),
      "$gen2ToInf =?>take(10) =?> replace(fn($x){$x gt 10}, ""Replacement"") =?> to-array()",
      $gen2ToInf =?>take(10) =?> replace(fn($x){$x gt 10}, "Replacement") =?> to-array(),
      "$gen2ToInf =?>take(10) =?> replace(fn($x){$x gt 11}, ""Replacement"") =?> to-array()",
      $gen2ToInf =?>take(10) =?> replace(fn($x){$x gt 11}, "Replacement") =?> to-array(),
      "$gen2ToInf =?>take(10) =?> replace(fn($x){$x lt 2}, ""Replacement"") =?> to-array()",
      $gen2ToInf =?>take(10) =?> replace(fn($x){$x lt 2}, "Replacement") =?> to-array(),
      "==> $gen2ToInf =?> replace(fn($x){$x gt 4}, ""Replacement"") =?> take(10) =?> to-array()",
      $gen2ToInf =?> replace(fn($x){$x gt 4}, "Replacement") =?> take(10) =?> to-array(),
      "$gen2ToInf =?> replace(fn($x){$x lt 3}, ""Replacement"") =?> take(10) =?> to-array()",
      $gen2ToInf =?> replace(fn($x){$x lt 3}, "Replacement") =?> take(10) =?> to-array(),
    (:  
      Will result in endless loop:
      
      , "==>  ==>  ==>  $gen2ToInf =?> replace(fn($x){$x lt 2}, ""Replacement"") =?> take(10) =?> to-array() <==  <==  <==",
      $gen2ToInf?replace2(fn($x){$x lt 2}, "Replacement") =?> take(10) =?> to-array() 
    :)
    "================",
    "$gen2ToInf =?> empty-generator() =?> reverse() =?> to-array()",
    $gen2ToInf =?> empty-generator() =?> reverse() =?> to-array(),
    "$gen2ToInf =?> empty-generator() =?> append(2) =?> reverse() =?> to-array()",
    $gen2ToInf =?> empty-generator() =?> append(2) =?> reverse() =?> to-array(),
    "$gen2ToInf =?>take(10) =?> reverse() =?> to-array()",
    $gen2ToInf =?>take(10) =?> reverse() =?> to-array(),
    "================",
    "$genN =?> take(10) =?> contains(3)",
    $genN =?> take(10) =?> contains(3),
    "$genN =?> take(10) =?> contains(20)",
    $genN =?> take(10) =?> contains(20),
    "$genN =?> take(10) =?> contains(1)",    
    $genN =?> take(10) =?> contains(1), 
    "$genN =?> take(10) =?> contains(10)",     
    $genN =?> take(10) =?> contains(10),  
    "$genN =?> take(10) =?> contains(0)",
    $genN =?> take(10) =?> contains(0), 
    "$genN =?> take(10) =?> contains(11)",        
    $genN =?> take(10) =?> contains(11),
    "==> $genN =?> contains(15)",    
    $genN =?> contains(15), 
    "================",
    "$gen2ToInf =?>take(5) =?> fold-left(0, fn($x, $y){$x + $y})",
    $gen2ToInf =?>take(5) =?> fold-left(0, fn($x, $y){$x + $y}),
    "================",
    "$gen2ToInf =?>take(5) =?> fold-right(0, fn($x, $y){$x + $y})",
    $gen2ToInf =?>take(5) =?> fold-right(0, fn($x, $y){$x + $y}),
    "================",
    "==> $gen0toInf =?> for-each(fn($n){(2 * $n + 1) div $factorial(2*xs:decimal($n)})
              =?> take(8) =?> fold-left(0, fn($x, $y){$x + $y})",
    $gen0toInf =?> for-each(fn($n){(2*$n + 1) div $factorial(2*xs:decimal($n))}) =?> take(8) =?> fold-left(0, fn($x, $y){$x + $y}),
    "================",    
    "$gen0toInf =?> for-each(fn($n){(2*$n + 1) div $factorial(2*xs:decimal($n))}) =?> take(8) =?> scan-left(0, fn($x, $y){$x + $y}) =?> to-array()",
    $gen0toInf =?> for-each(fn($n){(2*$n + 1) div $factorial(2*xs:decimal($n))}) =?> take(8) =?> scan-left(0, fn($x, $y){$x + $y}) =?> to-array(),
    "================",
    "let $genSeqE := $gen0toInf =?> for-each(fn($n){(2*$n + 1) div $factorial(2*xs:decimal($n))}) =?> take(8) =?> scan-left(0, fn($x, $y){$x + $y}),
    $genSeqE-Next := $genSeqE =?> tail(),
    $genZipped := $genSeqE =?> zip($genSeqE-Next)
 return
    $genZipped =?> first-where(fn($pair){abs($pair(1) - $pair(2)) lt 0.000001})(2)",
    let $genSeqE := $gen0toInf =?> for-each(fn($n){(2*$n + 1) div $factorial(2*xs:decimal($n))}) =?> take(8) =?> scan-left(0, fn($x, $y){$x + $y}),
        $genSeqE-Next := $genSeqE =?> tail(),
        $genZipped := $genSeqE =?> zip($genSeqE-Next)
      return
        $genZipped =?> first-where(fn($pair){abs($pair(1) - $pair(2)) lt 0.000001})(2),        
    "================",
    
    "$gen2ToInf =?> empty-generator() =?> scan-left(0, fn($x, $y){$x + $y}) =?> to-array()",
    $gen2ToInf =?> empty-generator() =?> scan-left(0, fn($x, $y){$x + $y}) =?> to-array(),
    "$gen2ToInf =?>take(5) =?> scan-left(0, fn($x, $y){$x + $y}) =?> to-array()",
    $gen2ToInf =?>take(5) =?> scan-left(0, fn($x, $y){$x + $y}) =?> to-array(),
    "================",
    "$gen2ToInf =?> make-generator-from-sequence((1 to 10)) =?> scan-right(0, fn($x, $y){$x + $y}) =?> to-array()",
    $gen2ToInf =?> make-generator-from-sequence((1 to 10)) =?> scan-right(0, fn($x, $y){$x + $y}) =?> to-array(),
    "================",
    let $multShortCircuitProvider := fn($x, $y)
        {
          if($x eq 0) then fn(){0}
            else fn($z) {$x * $z}
        },
        $gen-5ToInf := $gen2ToInf =?> for-each(fn($n){$n -7})
     return
     (
       "let $multShortCircuitProvider := fn($x, $y)
        {
          if($x eq 0) then fn(){0}
            else fn($z) {$x * $z}
        },
            $gen-5ToInf := $gen2ToInf =?> for-each(fn($n){$n -7})
          return
            $gen2ToInf =?>take(5) =?> fold-lazy(1, $product, $multShortCircuitProvider),
            $gen-5ToInf =?> fold-lazy(1, $product, $multShortCircuitProvider)",
       $gen2ToInf =?>take(5) =?> fold-lazy(1, $product, $multShortCircuitProvider),
       $gen-5ToInf =?> fold-lazy(1, $product, $multShortCircuitProvider)
     ),
     "===============",
     "     let $myMap := {'John': 22, 'Ann': 28, 'Peter': 31}
      return 
        $gen2ToInf =?> make-generator-from-map($myMap) =?> to-array()",
     let $myMap := {"John": 22, "Ann": 28, "Peter": 31}
      return 
        $gen2ToInf =?> make-generator-from-map($myMap) =?> to-array(),
     "===============",        
     "let $myMap := {'John': 22, 'Ann': 28, 'Peter': 31},
          $genMap := $gen2ToInf =?> make-generator-from-map($myMap)
      return
        $genMap =?> to-map()" ,
     let $myMap := {"John": 22, "Ann": 28, "Peter": 31},
         $genMap := $gen2ToInf =?> make-generator-from-map($myMap)
      return
        $genMap =?> to-map(),
     "$gen2ToInf =?>take(10) =?> to-map()",
     $gen2ToInf =?>take(10) =?> to-map()         
   )
