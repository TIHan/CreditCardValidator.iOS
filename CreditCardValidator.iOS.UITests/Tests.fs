namespace CreditCardValidator.iOS.UITests

open NUnit.Framework
open Xamarin.UITest
open Xamarin.UITest.Queries

module DSL =

    type UIQuery = private UIQuery of (AppQuery -> AppQuery)

    let runUIQuery = function
        | UIQuery f -> f

    let (==>) q1 q2 =
        match q1, q2 with
        | UIQuery f1, UIQuery f2 ->
            UIQuery (fun x ->
                f2 ((f1 x))
            )

    let klass name =
        UIQuery (fun x -> x.Class name)

    let marked name =
        UIQuery (fun x -> x.Marked name)

    type UITest<'T> = private UITest of (IApp -> 'T)

    let runUITest app = function
        | UITest f -> f app |> ignore

    let waitForElement (q : UIQuery) =
        match q with
        | UIQuery f ->
            UITest (fun app -> app.WaitForElement f)

    let (>>=) (t : UITest<'a>) (f : 'a -> UITest<'b>) =
        match t with
        | UITest tf ->
            UITest (fun app ->
                match f (tf app) with
                | UITest tf2 -> tf2 app
            )

    let enterText text q =
        match q with
        | UIQuery f ->
            UITest (fun app -> 
                app.EnterText(f, text)
            )

    let tap q =
        match q with
        | UIQuery f ->
            UITest (fun app -> app.Tap f)

open DSL

type Tests() = 

    [<TestCase(Platform.iOS)>]
    member this.AppLaunches(platform : Platform) = 
        let platform = platform
        let app = AppInitializer.startApp (platform)

        let test =
            klass "UINavigationBar"
            ==> marked "Simple Credit Card Validator"
            |> waitForElement >>= fun _ ->

                klass "UITextField"
                |> enterText "999999999999999" >>= fun _ ->

                    marked "Validate Credit Card"
                    ==> klass "UIButton"
                    |> tap >>= fun _ ->

                        marked "Credit card number is too short."
                        ==> klass "UILabel"
                        |> waitForElement

        runUITest app test

        //app.WaitForElement(fun c -> c.Class("UINavigationBar").Marked("Simple Credit Card Validator"))
        //|> ignore

        //app.EnterText((fun c -> c.Class("UITextField")), "999999999999999")
        //app.Tap(fun c -> c.Marked("Validate Credit Card").Class("UIButton"))

        //app.WaitForElement(fun c -> c.Marked("Credit card number is too short.").Class("UILabel"))
        //|> ignore

        ()

