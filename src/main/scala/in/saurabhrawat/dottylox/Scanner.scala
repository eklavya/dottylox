package in.saurabhrawat.dottylox

import java.util.ArrayList

class Scanner(source: String):
    val tokens = new ArrayList[Token]()

    var start = 0
    var current = 0
    var line = 1

    def scanTokens() =
        while !isAtEnd() do
            start = current
            scanToken()
        tokens.add(Token(TokenType.EOF, "", None, line))
        tokens

    def isAtEnd() = 
        current >= source.length
    
    def scanToken() = 
        val c = advance()
        c match
            case '(' => addToken(TokenType.LEFT_PAREN)
            case ')' => addToken(TokenType.RIGHT_PAREN)
            case '{' => addToken(TokenType.LEFT_BRACE)
            case '}' => addToken(TokenType.RIGHT_BRACE)
            case ',' => addToken(TokenType.COMMA)
            case '.' => addToken(TokenType.DOT)
            case '-' => addToken(TokenType.MINUS)
            case '+' => addToken(TokenType.PLUS)
            case ';' => addToken(TokenType.SEMICOLON)
            case '*' => addToken(TokenType.STAR)
            case '!' => addToken(if matchChar('=') then TokenType.BANG_EQUAL else TokenType.BANG)
            case '=' => addToken(if matchChar('=') then TokenType.EQUAL_EQUAL else TokenType.EQUAL)
            case '<' => addToken(if matchChar('=') then TokenType.LESS_EQUAL else TokenType.LESS)
            case '>' => addToken(if matchChar('=') then TokenType.GREATER_EQUAL else TokenType.GREATER)
            case '/' => 
                if matchChar('/')
                    comment()
                else if matchChar('*')
                    blockComment()
                else
                    addToken(TokenType.SLASH)
            case '?' => addToken(TokenType.QUESTION)
            case ':' => addToken(TokenType.COLON)
            case '\n' =>
                line += 1
            case ' ' | '\t' | '\r' =>
            case '"' => string()
            case d if isDigit(d) => number()
            case a if isAlpha(a) => identifier()
            case _ => Main.error(line, "Unexpected character")


    def isDigit(c: Char) = c.isDigit

    def isAlpha(c: Char) = c.isLetter

    def isAlphaNumeric(c: Char) = c.isLetterOrDigit

    def matchChar(c: Char) = 
        if isAtEnd()
            false
        else if source.charAt(current) != c
            false
        else
            current += 1
            true

    def peek(): Char =
        if isAtEnd() 
            '\0'
        else
            source.charAt(current) 

    def advance() = 
        current += 1
        source.charAt(current - 1)

    def addToken(tokenType: TokenType): Unit =
        addToken(tokenType, None)

    def addToken(tokenType: TokenType, literal: Option[LiteralType]) =
        val text = source.substring(start, current)
        tokens.add(Token(tokenType, text, literal, line))

    def string(): Unit =
        while peek() != '"' && !isAtEnd() do
            if peek() == '\n'
                line += 1
            advance()
        if isAtEnd()
            Main.error(line, "Unterminated string.")
        
        advance()
        val str = source.substring(start + 1, current - 1)
        addToken(TokenType.STRING, Some(str))

    def number(): Unit =
        while isDigit(peek()) && !isAtEnd() do advance()
        if peek() == '.' && isDigit(peekNext())
            advance()
            while isDigit(peek()) do advance()
            addToken(TokenType.NUMBER, Some(source.substring(start, current).toDouble))
        else if start != current
            addToken(TokenType.NUMBER, Some(source.substring(start, current).toInt))

    def peekNext(): Char =
        if current + 1 >= source.length then '\0'
        else source.charAt(current + 1)
        
    def identifier(): Unit =
        while isAlphaNumeric(peek()) && !isAtEnd() do advance()
        val identifier = source.substring(start, current)
        val tokenType = Scanner.keywords.get(identifier).getOrElse(TokenType.IDENTIFIER)
        addToken(tokenType)

    def comment(): Unit =
        while peek() != '\n' && !isAtEnd() do advance()
        addToken(TokenType.COMMENT, Some(source.substring(start, current)))

    def blockComment(): Unit =
        while !(peek() == '*' && peekNext() == '/') && !isAtEnd() do 
            advance()
            if matchChar('\n') then line += 1
        
        // consume ending */
        if !isAtEnd()
            advance()
            advance()
        
        addToken(TokenType.COMMENT, Some(source.substring(start, current)))


object Scanner:

    val keywords = Map(
        "and" ->    TokenType.AND,                       
        "class" ->  TokenType.CLASS,                     
        "else" ->   TokenType.ELSE,                      
        "false" ->  TokenType.FALSE,                     
        "for" ->    TokenType.FOR,                       
        "fun" ->    TokenType.FUN,                       
        "if" ->     TokenType.IF,                        
        "nil" ->    TokenType.NIL,                       
        "or" ->     TokenType.OR,                        
        "print" ->  TokenType.PRINT,                     
        "return" -> TokenType.RETURN,                    
        "super" ->  TokenType.SUPER,                     
        "this" ->   TokenType.THIS,                      
        "true" ->   TokenType.TRUE,                      
        "var" ->    TokenType.VAR,                       
        "while" ->  TokenType.WHILE,   
    )


