



//(λ param . output)input => output [param := input] => result

/**
(λx.xy)z = (xy)[x:=z] = (zy) = zy

  param = x
  output = xy
  input= = z


  ((λx.(x y))(λz.z))


  The idea behing the lmba function is the substitution, so we can have a method
  to define function and study their interaction

  The same is what happen when I use the lambda expression with java or scala
  because i define an anonymous function that uses the input to create a value



   1) (
        (λx.(
            (λy.(x y))x
            )
        ) (λz.w) )
  2)  (
        (λx.(
            (λy.(x y))x
            )
        ) (λz.w) )


  **/

