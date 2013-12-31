package   model


import   model._
import   model.statement._

object StatementVisitor {
  val Skip = 0
  val Continue = 1
}

abstract class StatementVisitor {
  def visit(stmt: Statement): Int = { StatementVisitor.Continue }

  def visit(tu: TranslationUnit): Int = { StatementVisitor.Continue }
  def visit(function: FunctionDef): Int = { StatementVisitor.Continue }
  def visit(stmt: StatementBlock): Int = { StatementVisitor.Continue }
  def visit(stmt: FunctionCallStatement): Int = { StatementVisitor.Continue }
  def visit(stmt: DeclarationStatement): Int = { StatementVisitor.Continue }
  def visit(stmt: ExpressionStatement): Int = { StatementVisitor.Continue } //{ throw new UnsupportedOperationException("Unexpected expr stmt: " + PrettyPrinter.pretty(stmt)) }
  def visit(stmt: ForStatement): Int = { StatementVisitor.Continue }
  def visit(stmt: IfStatement): Int = { StatementVisitor.Continue }
  def visit(stmt: WhileStatement): Int = { StatementVisitor.Continue }
  def visit(stmt: DoStatement): Int = { StatementVisitor.Continue }
  def visit(stmt: SwitchStatement): Int = { StatementVisitor.Continue }
  def visit(stmt: CaseStatement): Int = { StatementVisitor.Continue }
  def visit(stmt: ReturnStatement): Int = { StatementVisitor.Continue }
  def visit(stmt: BreakStatement): Int = { StatementVisitor.Continue }
  def visit(stmt: ContinueStatement): Int = { StatementVisitor.Continue }
  def visit(stmt: DefaultStatement): Int = { StatementVisitor.Continue }
  def visit(stmt: NullStatement): Int = { StatementVisitor.Continue }
  def visit(stmt: GotoStatement): Int = { StatementVisitor.Continue }
  def visit(stmt: LabelStatement): Int = { StatementVisitor.Continue }
  def visit(stmt: AssignmentStatement): Int = { StatementVisitor.Continue }

  def leave(tu: TranslationUnit): Unit = {}
  def leave(function: FunctionDef): Unit = {}
  def leave(stmt: StatementBlock): Unit = {}
  def leave(stmt: FunctionCallStatement): Unit = {}
  def leave(stmt: DeclarationStatement): Unit = {}
  def leave(stmt: ExpressionStatement): Unit = {}
  def leave(stmt: ForStatement): Unit = {}
  def leave(stmt: IfStatement): Unit = {}
  def leave(stmt: WhileStatement): Unit = {}
  def leave(stmt: DoStatement): Unit = {}
  def leave(stmt: SwitchStatement): Unit = {}
  def leave(stmt: CaseStatement): Unit = {}
  def leave(stmt: ReturnStatement): Unit = {}
  def leave(stmt: BreakStatement): Unit = {}
  def leave(stmt: ContinueStatement): Unit = {}
  def leave(stmt: DefaultStatement): Unit = {}
  def leave(stmt: NullStatement): Unit = {}
  def leave(stmt: GotoStatement): Unit = {}
  def leave(stmt: LabelStatement): Unit = {}  
  def leave(stmt: AssignmentStatement): Unit = {}

}