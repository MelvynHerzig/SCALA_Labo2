package Chat

import Chat.Token.*
import Utils.SpellCheckerService

class TokenizerService(spellCheckerSvc: SpellCheckerService):

  /**
    * Function that return a token according to a word.
    * @param word Word to translate into a token.
    * @return A value from Token enum.
    */
  def getCorrespondingToken(word : String) : Token =
    word match {
      case "bonjour" => Token.BONJOUR
      case "je"=> Token.JE
      case "etre" => Token.ETRE
      case "vouloir" => Token.VOULOIR
      case "assoiffe" => Token.ASSOIFFE
      case "affame" => Token.AFFAME
      case "biere" => Token.PRODUCT
      case "croissant" => Token.PRODUCT
      case "et" => Token.ET
      case "ou" => Token.OU
      case "svp" => Token.SVP
      case a if spellCheckerSvc.isPseudonym(a) => Token.PSEUDO
      case a if spellCheckerSvc.isNumber(a) => Token.NUM
      case _ => Token.UNKNOWN
    }
  end getCorrespondingToken

  /**
    * Separate the user's input into tokens
    * @param input The user's input
    * @return A Tokenizer which allows iteration over the tokens of the input
    */
  def tokenize(input: String): Tokenized =

    // Removing punctuation and trimming then split by space.
    val words = input.replaceAll("[.,!?*]", "")
      .replaceAll("[’']", " ")
      .trim.replaceAll(" +", " ")
      .split(" ")

    var tokens : Array[(String, Token)] = Array()

    // For each word, get the corresponding word from dictionary and add the pair into the tokens array. 
    for word <- words do
      val realWord : String = spellCheckerSvc.getClosestWordInDictionary(word)
      tokens = tokens :+ (realWord, getCorrespondingToken(realWord))

    TokenizedImpl(tokens)
  end tokenize

end TokenizerService
