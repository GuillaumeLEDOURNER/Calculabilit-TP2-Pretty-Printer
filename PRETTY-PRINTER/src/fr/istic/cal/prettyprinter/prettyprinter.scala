package fr.istic.cal.prettyprinter

import scala.util.Try

/**
* définition d'une exception pour le cas des listes vides de commandes
*/
  case object ExceptionListeVide extends Exception
  
object Prettyprinter{
  
  /**
   * UTILISATION D'UN ANALYSEUR SYNTAXIQUE POUR LE LANGAGE WHILE
   *
   * les 3 fonctions suivantes permettent de construire un arbre de syntaxe abstraite 
   * respectivement pour une expression, une commande, un programme
   */
  
  /**
   * @param s : une chaine de caractère représentant la syntaxe concrète d'une expression du langage WHILE
   * @return un arbre de syntaxe abstraite pour cette expression
   */
  def readWhileExpression(s : String) : Expression = { WhileParser.analyserexpression(s)}
  
  /**
   * @param s : une chaine de caractère représentant la syntaxe concrète d'une commande du langage WHILE
   * @return un arbre de syntaxe abstraite pour cette commande
   */
  def readWhileCommand(s : String) : Command= {WhileParser.analysercommand(s)}  
  
  /**
   * @param s : une chaine de caractère représentant la syntaxe concrète d'un programme du langage WHILE
   * @return un arbre de syntaxe abstraite pour ce programme
   */
  def readWhileProgram(s : String) : Program = {WhileParser.analyserprogram(s)}

  
  
  /**
   * UN PRETTY-PRINTER POUR LE LANGAGE WHILE
   *
   */

  
  /**
   * définition d'un type pour les spécifications d'indentation
   */
  type IndentSpec = List[(String, Int)]
  

  /**
   *  TRAITEMENT DES EXPRESSIONS DU LANGAGE WHILE
   */

  /**
   * @param expression : un AST décrivant une expression du langage WHILE
   * @return une chaîne représentant la syntaxe concrète de l'expression
   */
  // TODO TP2
  def prettyPrintExpr(expression: Expression): String = { 
    expression match {
      case Nl => "nil"  
      case Cst(x) => x
      case VarExp(x) => x
      case Cons(x,y) => "(cons "+prettyPrintExpr(x)+" "+prettyPrintExpr(y)+")"
      case Hd(x) => "(hd "+ prettyPrintExpr(x)+")"
      case Eq(x,y) => prettyPrintExpr(x) + " =? " + prettyPrintExpr(y)
      case Tl(x) => "(tl "+prettyPrintExpr(x)+")"
    }
  }
  

  
  /**
   *  FONCTIONS AUXILIAIRES DE TRAITEMENT DE CHAINES POUR L'INDENTATION DES COMMANDES
   *  OU LA PRESENTATION DU PROGRAMME
   */

  /**
   * * définition d'une valeur d'indentation par défaut
   */
   // TODO TP2
   val indentDefault : Int = 1
  
  /**
   * recherche d'une valeur d'indentation dans une liste de spécifications d'indentation
   *
   * @param context une chaîne de caractères décrivant un contexte d'indentation
   * @param is une liste de spécifications d'indentation, chaque spécification étant un couple (contexte,indentation)
   * @return l'indentation correspondant à contexte
   */
  // TODO TP2
  def indentSearch(context: String, is: IndentSpec): Int = { 
     is match {
       case Nil => indentDefault
       case (x,y)::reste => 
         if(x == context){ 
           y
         } else {
          indentSearch(context, reste) 
         }
 
     }
   }

  
  /**
   * création d'une indentation
   *
   * @param n un nombre d'espaces
   * @return une chaîne de n espaces
   */
  // TODO TP2
  def makeIndent(n: Int): String = { 
    n match {
      case 0 => ""
      case 1 => " "
      case _ => " " + makeIndent(n-1)
    }
  }

  
  /**
   * ajout d'une chaîne devant chaque élément d'une liste de chaînes
   *
   * @param pref une chaîne
   * @param strings une liste de chaînes
   * @return une liste de chaînes obtenue par la concaténation de pref devant chaque élément de strings
   */
  //TODO TP2
  def appendStringBeforeAll(pref: String, strings: List[String]): List[String] = { 
    strings match {
      case Nil => Nil
      case a::Nil => pref+a::Nil
      case a::reste => pref+a::appendStringBeforeAll(pref, reste)
    }
  }

  
  /**
   * ajout d'une chaîne après chaque élément d'une liste de chaînes
   *
   * @param suff une chaîne
   * @param strings une liste de chaînes
   * @return une liste de chaînes obtenue par la concaténation de pref après chaque élément de strings
   */
  //TODO TP2
  def appendStringAfterAll(suff: String, strings: List[String]): List[String] = { 
     strings match {
      case Nil => Nil 
      case a::reste => a+suff::appendStringAfterAll(suff, reste)
  }
  }

  
  /**
   * ajout d'une chaîne après chaque élément d'une liste de chaînes sauf le dernier
   *
   * @param suff une chaîne
   * @param strings une liste non vide de chaînes
   * @return une liste de chaînes obtenue par la concaténation de pref après chaque élément de strings
   *         sauf le dernier
   */
  //TODO TP2
  def appendStringAfterAllButLast(suff: String, strings: List[String]): List[String] = { 
    strings match {
      case Nil => Nil
      case a::Nil => strings
      case a::b::Nil => a::suff+b::Nil
      case a::reste => a+suff::appendStringAfterAllButLast(suff, reste)
    }
  }

  
  /**
   * ajout d'une chaîne après le dernier élément d'une liste de chaînes
   *
   * @param suff une chaîne
   * @param strings une liste non vide de chaînes
   * @return une liste de chaînes obtenue par la concaténation de pref après le dernier élément de strings
   */
  //TODO TP2
  def appendStringAfterLast(suff: String, strings: List[String]): List[String] = { 
    strings match {
      case Nil => suff::Nil
      case a::Nil => a+suff::Nil
      case a::reste => a::appendStringAfterLast(suff, reste)
    }
  }
  
  
  /**
   *
   *  TRAITEMENT DES COMMANDES DU LANGAGE WHILE
   */

  /**
   * @param command : un AST décrivant une commande du langage WHILE
   * @param is : une liste de spécifications d'indentation
   * @return une liste de chaînes représentant la syntaxe concrète de la commande
   */
  // TODO TP2
  def prettyPrintCommand(command: Command, is: IndentSpec): List[String] = { 
    command match {
      case Nop => "nop"::Nil
      case Set(Var(v),e) => v +" := "+ prettyPrintExpr(e) :: Nil
      case While(condition,body) => List("while "+prettyPrintExpr(condition)+" do") ++ prettyPrintCommands(body,is) ++ List("od") 
    }
  }
    
  
  /**
   * @param commands : une liste d'AST décrivant une liste de commandes du langage WHILE
   * @param is : une liste de spécifications d'indentation
   * @return une liste de chaînes représentant la syntaxe concrète de la listes de commandes
   */
  // TODO TP2
  def prettyPrintCommands(commands: List[Command], is: IndentSpec): List[String] = {
    commands match {
      case Nil => Nil
      case c::Nil => appendStringBeforeAll(makeIndent(indentSearch("WHILE",is)),prettyPrintCommand(c,is))
      case c::reste => val command_c =  prettyPrintCommand(c,is) ; 
                       val command_reste = prettyPrintCommands(reste, is) ; 
                       val command = command_c++command_reste
                       appendStringAfterAllButLast(";",(appendStringBeforeAll(makeIndent(indentSearch("WHILE",is)), command)))
    }
  }
  
  
 /**
   *
   *  TRAITEMENT DES PROGRAMMES DU LANGAGE WHILE
   */

  /**
   * @param vars : une liste décrivant les paramètres d'entrée d'un programme du langage WHILE
   * @return une liste de chaînes représentant la syntaxe concrète des paramètres d'entrée du programme
   */
 // TODO TP2
 def prettyPrintIn(vars : List[Variable]): String = { ??? }
 
 
  /**
   * @param vars : une liste décrivant les paramètres de sortie d'un programme du langage WHILE
   * @return une liste de chaînes représentant la syntaxe concrète des paramètres de sortie du programme
   */
 // TODO TP2
 def prettyPrintOut(vars : List[Variable]): String = { ??? }
 
 
  /**
   * @param program : un AST décrivant un programme du langage WHILE
   * @param is : une liste de spécifications d'indentation
   * @return une liste de chaînes représentant la syntaxe concrète du programme
   */
  // TODO TP2
  def prettyPrintProgram(program : Program, is: IndentSpec): List[String] = { ??? }   
        
  
 /**
   * @param program : un AST décrivant un programme du langage WHILE
   * @param is : une liste de spécifications d'indentation
   * @return une chaîne représentant la syntaxe concrète du programme
   */
  // TODO TP2
  def prettyPrint(program : Program, is: IndentSpec): String = { ??? }
 
 val program: Program =
    Progr(
      List(Var("X")),
      List(
        Set(Var("Y"), Nl),
        While(
          VarExp("X"),
          List(
            Set(Var("Y"), Cons(Hd(VarExp("X")), VarExp("Y"))),
            Set(Var("X"), Tl(VarExp("X")))))),
      List(Var("Y")));" "
  val is: IndentSpec = List(("PROGR", 2), ("WHILE", 5));

 def main(args: Array[String]): Unit = {
   
   // vous pouvez ici tester manuellement vos fonctions par des print
   
   
 }
}