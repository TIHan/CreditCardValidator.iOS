namespace CreditCardValidator.iOS.UITests

open NUnit.Framework
open Xamarin.UITest
open Xamarin.UITest.Queries

module DSL =

    type UIQuery = private UIQuery of (AppQuery -> AppQuery)

    let runUIQuery appQuery = function
        | UIQuery f -> f appQuery

    let (==>) q1 q2 =
        match q1, q2 with
        | UIQuery f1, UIQuery f2 ->
            UIQuery (fun x ->
                f2 ((f1 x))
            )

    let Class name =
        UIQuery (fun x -> x.Class name)

    let Marked name =
        UIQuery (fun x -> x.Marked name)

    type UITest<'T> = private UITest of (IApp -> 'T)

    let runUITest app = function
        | UITest f -> f app |> ignore

    let WaitForElement (q : UIQuery) =
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

    let EnterText text q =
        match q with
        | UIQuery f ->
            UITest (fun app -> 
                app.EnterText(f, text)
            )

    let Tap q =
        match q with
        | UIQuery f ->
            UITest (fun app -> app.Tap f)

open DSL

type Tests() = 

    [<TestCase(Platform.iOS)>]
    member this.AppLaunches(platform : Platform) = 
        let platform = platform
        let app = AppInitializer.startApp (platform)

        let waitForPage =
            Class "UINavigationBar" ==> Marked "Simple Credit Card Validator"
            |> WaitForElement

        let enterCC =
            Class "UITextField"
            |> EnterText "999999999999999"

        let validateCC =
            Marked "Validate Credit Card" ==> Class "UIButton"
            |> Tap

        let waitForError =
            Marked "Credit card number is too short." ==> Class "UILabel"
            |> WaitForElement

        let test =
            Class "UINavigationBar" ==> Marked "Simple Credit Card Validator"
            |> WaitForElement >>= fun _ ->

                Class "UITextField"
                |> EnterText "999999999999999" >>= fun _ ->

                    Marked "Validate Credit Card" ==> Class "UIButton"
                    |> Tap >>= fun _ ->

                        Marked "Credit card number is too short." ==> Class "UILabel"
                        |> WaitForElement

        runUITest app test

        //app.WaitForElement(fun c -> c.Class("UINavigationBar").Marked("Simple Credit Card Validator"))
        //|> ignore

        //app.EnterText((fun c -> c.Class("UITextField")), "999999999999999")
        //app.Tap(fun c -> c.Marked("Validate Credit Card").Class("UIButton"))

        //app.WaitForElement(fun c -> c.Marked("Credit card number is too short.").Class("UILabel"))
        //|> ignore

        ()

