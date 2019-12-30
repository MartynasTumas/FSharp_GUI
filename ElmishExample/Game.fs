namespace Main

module Game =
    open Configuration
    open System


    let GetWord = 
        let random = Random()
        let index = random.Next(0, WORDS.Length)
        WORDS.[index]

    let CheckGuess (word:string) (c:string) =
        if CASE_SENSITIVE then word.Contains(c)
            else word.ToLower().Contains(c.ToLower())

    let rec GetHelp(word:List<char>) (guesses:List<string>) =
        match word, guesses with
        |h::t, _ when not(CheckGuess (String.Concat(Array.ofList(guesses))) (h.ToString())) -> h
        |h::t, _ when CheckGuess (String.Concat(Array.ofList(guesses))) (h.ToString()) -> GetHelp t guesses
        |h::t,[] -> h
        |h::_,_ -> h
        |_ -> raise (new InvalidOperationException("Can't get help"))

    let rec ReadInput(word:List<char>) (guesses:List<string>) =
        let key = Console.ReadKey()
        if key.Modifiers = ConsoleModifiers.Control && key.Key = ConsoleKey.H && HELP
            then 
                printf"HELP USED"; [GetHelp word guesses]
        elif 
            key.Key = ConsoleKey.Enter then []
        elif 
            key.KeyChar |> Char.IsLetterOrDigit then key.KeyChar :: ReadInput word guesses
        elif 
            key.KeyChar |> Char.IsWhiteSpace then key.KeyChar :: ReadInput word guesses
        else
            ReadInput word guesses


    let rec CorrectOrder (word:List<char>) (guesses:string) =
        match word with
        |h::t when ALLOW_BLANKS && h=' ' -> [' ']@ CorrectOrder t guesses
        |h::t when CheckGuess guesses (h.ToString()) -> [h]@ CorrectOrder t guesses
        |h::t when not(CheckGuess guesses (h.ToString())) -> [Char.Parse(HIDDEN)]@ CorrectOrder t guesses
        |_ -> []
    let rec GetCorrectGuesses (word:string) (guesses:List<string>) =
        match guesses with
        |h::t when CheckGuess word h -> [h]@ GetCorrectGuesses word t
        |h::t when not(CheckGuess word h) -> GetCorrectGuesses word t
        |_ -> []
   
    
    let word = GetWord; // Word(string) to guess which is randomly got from an array of words.
    let wordList = Seq.toList word; // Char list of word characters.
    let mutable guesses = [] // String list of guesses. It also includes guess when help is called.



    let PlayGame(inputString:string) =
        
        let input = Seq.toList inputString
        if(input.Length=1) then // if input is one character.
            if(List.contains (input.[0].ToString()) guesses = false) // if same guess is guessed multiple times, it doesn't add to the guesses list,
                                                                    // so same guess multiple times will count as an one guess.
                then
                    guesses <- input.[0].ToString() :: guesses // Add guess to the list.
        elif(input.Length>1 && MULTIPLE) then // If input contains multiple characters and program is set to allow multiple characters.
            if(List.contains (String.Concat(Array.ofList(input))) guesses = false)// Here it also checks if same guess already exists.
                                                        // Input is an char array of characters, so 
                                                        // before checking if guess already exists, it needs to add all characters in one string.
                then
                    guesses <- (String.Concat(Array.ofList(input))) :: guesses//Add guess to the list.
    
        let guessedWord = GetCorrectGuesses word guesses |> String.Concat // Returns a string of characters that are in guesses list AND in a word.
        let displayWord = CorrectOrder wordList guessedWord |> String.Concat // Returns guessedWord string in same order as an word that needs to be guessed.
                                                       // If guessedWord doesn't contain character(s) that is in the acutal word, it will be raplaced with hidden character.
        printf ""// form.Controls.Find("display",true) = "aa"
        if(displayWord.Contains(HIDDEN))// If displayWord contains hidden characters, program will print word and start loop again.
        then
            displayWord           
        else// If there is no more hidden characters in the word it means that the word was guessed and game is finished.
            "You won. Used guesses: " + guesses.Length.ToString()


    type Model =
      { 
        DisplayedWord: string
        Input: string }

    let init () =
      { DisplayedWord = "The lenght of word: " + wordList.Length.ToString()
        Input = "" }

    type Msg =
    | GetInput of string
    | Display
    | Guess

    let update msg m =
      match msg with
      | GetInput x -> { m with Input = x }
      | Guess -> {m with DisplayedWord = m.DisplayedWord + "\r\n" + PlayGame m.Input}
     // | Guess -> { m with DisplayedWord = PlayGame m.GuessedWord}

    open Elmish.WPF
    open ElmishExample.View
    
    let bindings () : Binding<Model, Msg> list =
      [
        "Display" |> Binding.oneWay (fun m -> m.DisplayedWord)
        "Guess" |> Binding.cmd (fun m -> Guess)
   //     "Input" |> Binding.oneWay (fun m -> m.Input)
        "Input" |> Binding.twoWay (
            (fun m -> string m.Input),
            (fun newVal m -> string newVal |> GetInput))
      ]



