#include "tokenizer.hpp"

namespace Tokenizer {

std::shared_ptr<Tokenizer> createTokenizer() {
    auto rules = std::make_shared<TokenDefinitionList>();
    rules->push_back(std::make_shared<TokenDefinitionBuilder>()
                         ->name("WHITESPACE")
                         ->appendOneOrMore()
                         ->whitespace()
                         ->build());
    rules->push_back(std::make_shared<TokenDefinitionBuilder>()
                         ->name("IDENFR")
                         ->priority(1)
                         ->appendOne()
                         ->letter()
                         ->character('_')
                         ->appendArbitraryNumberOf()
                         ->letter()
                         ->character('_')
                         ->number()
                         ->build());
    rules->push_back(std::make_shared<TokenDefinitionBuilder>()
                         ->name("INTCON")
                         ->appendOneOrMore()
                         ->number()
                         ->build());
    rules->push_back(std::make_shared<TokenDefinitionBuilder>()
                         ->name("STRCON")
                         ->appendOne()
                         ->character('\"')
                         ->appendArbitraryNumberOf()
                         ->subDefinition(
                             std::make_shared<TokenDefinitionBuilder>()
                                 ->appendOne()
                                 ->anyCharacter()
                                 ->except('\"')
                                 ->except('\\')
                                 ->except('\n')
                                 ->build())
                         ->subDefinition(
                             std::make_shared<TokenDefinitionBuilder>()
                                 ->appendOne()
                                 ->character('\\')
                                 ->appendOne()
                                 ->anyCharacter()
                                 ->build())
                         ->appendOne()
                         ->character('\"')
                         ->build());
    rules->push_back(std::make_shared<TokenDefinitionBuilder>()
                         ->name("COMMENT")
                         ->appendOne()
                         ->subDefinition(
                             std::make_shared<TokenDefinitionBuilder>()
                                 ->appendOne()
                                 ->character('/')
                                 ->appendOne()
                                 ->character('/')
                                 ->appendArbitraryNumberOf()
                                 ->anyCharacter()
                                 ->except('\n')
                                 ->appendOne()
                                 ->character('\n')
                                 ->build())
                         ->subDefinition(
                             std::make_shared<TokenDefinitionBuilder>()
                                 ->appendOne()
                                 ->character('/')
                                 ->appendOne()
                                 ->character('*')
                                 ->appendArbitraryNumberOf()
                                 ->anyCharacter()
                                 ->except('*')
                                 ->appendArbitraryNumberOf()
                                 ->subDefinition(std::make_shared<TokenDefinitionBuilder>()
                                                     ->appendOne()
                                                     ->character('*')
                                                     ->appendOne()
                                                     ->anyCharacter()
                                                     ->except('/')
                                                     ->appendArbitraryNumberOf()
                                                     ->anyCharacter()
                                                     ->except('*')
                                                     ->build())
                                 ->appendOne()
                                 ->character('*')
                                 ->appendOne()
                                 ->character('/')
                                 ->build())
                         ->build());
    auto declare = [&](std::string name, std::string content) {
        rules->push_back(std::make_shared<TokenDefinitionBuilder>()
                             ->name(name)
                             ->appendString(content)
                             ->build());
    };
    declare("MAINTK", "main");
    declare("CONSTTK", "const");
    declare("INTTK", "int");
    declare("BREAKTK", "break");
    declare("CONTINUETK", "continue");
    declare("IFTK", "if");
    declare("ELSETK", "else");
    declare("NOT", "!");
    declare("AND", "&&");
    declare("OR", "||");
    declare("WHILETK", "while");
    declare("GETINTTK", "getint");
    declare("PRINTFTK", "printf");
    declare("RETURNTK", "return");
    declare("PLUS", "+");
    declare("MINU", "-");
    declare("VOIDTK", "void");
    declare("MULT", "*");
    declare("DIV", "/");
    declare("MOD", "%");
    declare("LSS", "<");
    declare("LEQ", "<=");
    declare("GRE", ">");
    declare("GEQ", ">=");
    declare("EQL", "==");
    declare("NEQ", "!=");
    declare("ASSIGN", "=");
    declare("SEMICN", ";");
    declare("COMMA", ",");
    declare("LPARENT", "(");
    declare("RPARENT", ")");
    declare("LBRACK", "[");
    declare("RBRACK", "]");
    declare("LBRACE", "{");
    declare("RBRACE", "}");
    auto tokenizer = std::make_shared<Tokenizer>(rules);
    return tokenizer;
}

}  // namespace Tokenizer
