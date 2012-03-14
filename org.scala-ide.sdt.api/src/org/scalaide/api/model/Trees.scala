package org.scalaide.api.model

import scala.collection.mutable.ListBuffer

trait Trees { self: Universe =>
  type Modifiers >: Null <: AbsModifiers

  abstract class AbsModifiers {
    def modifiers: Set[Modifier]
    def hasModifier(mod: Modifier): Boolean
    def privateWithin: Name  // default: EmptyTypeName
    def annotations: List[Tree] // default: List()
    def mapAnnotations(f: List[Tree] => List[Tree]): Modifiers
  }

  def Modifiers(mods: Set[Modifier] = Set(),
                privateWithin: Name = EmptyTypeName,
                annotations: List[Tree] = List()): Modifiers

  /** Tree is the basis for scala's abstract syntax. The nodes are
   *  implemented as case classes, and the parameters which initialize
   *  a given tree are immutable: however Trees have several mutable
   *  fields which are manipulated in the course of typechecking,
   *  including pos, symbol, and tpe.
   *
   *  Newly instantiated trees have tpe set to null (though it
   *  may be set immediately thereafter depending on how it is
   *  constructed.) When a tree is passed to the typer, typically via
   *  `typer.typed(tree)`, under normal circumstances the tpe must be
   *  null or the typer will ignore it. Furthermore, the typer is not
   *  required to return the same tree it was passed.
   *
   *  Trees can be easily traversed with e.g. foreach on the root node;
   *  for a more nuanced traversal, subclass Traverser. Transformations
   *  can be considerably trickier: see the numerous subclasses of
   *  Transformer found around the compiler.
   *
   *  Copying Trees should be done with care depending on whether
   *  it need be done lazily or strictly (see LazyTreeCopier and
   *  StrictTreeCopier) and on whether the contents of the mutable
   *  fields should be copied. The tree copiers will copy the mutable
   *  attributes to the new tree; calling Tree#duplicate will copy
   *  symbol and tpe, but all the positions will be focused.
   *
   *  Trees can be coarsely divided into four mutually exclusive categories:
   *
   *  - TermTrees, representing terms
   *  - TypTrees, representing types.  Note that is `TypTree`, not `TypeTree`.
   *  - SymTrees, which may represent types or terms.
   *  - Other Trees, which have none of those as parents.
   *
   *  SymTrees include important nodes Ident and Select, which are
   *  used as both terms and types; they are distinguishable based on
   *  whether the Name is a TermName or TypeName.  The correct way for
   *  to test for a type or a term (on any Tree) are the isTerm/isType
   *  methods on Tree.
   *
   *  "Others" are mostly syntactic or short-lived constructs. Examples
   *  include CaseDef, which wraps individual match cases: they are
   *  neither terms nor types, nor do they carry a symbol. Another
   *  example is Parens, which is eliminated during parsing.
   */
  abstract class Tree extends Product {

    /** The source position of this tree. */
    val pos: Position
    
    /** The type of this tree, if it has been already typed, 'null' otherwise. */
    val tpe: Type

    /** Note that symbol is fixed as null at this level.  In SymTrees,
     *  it is overridden and implemented with a var, initialized to NoSymbol.
     *
     *  Trees which are not SymTrees but which carry symbols do so by
     *  overriding `def symbol` to forward it elsewhere.  Examples:
     *
     *    Super(qual, _)              // has qual's symbol
     *    Apply(fun, args)            // has fun's symbol
     *    TypeApply(fun, args)        // has fun's symbol
     *    AppliedTypeTree(tpt, args)  // has tpt's symbol
     *    TypeTree(tpe)               // has tpe's typeSymbol, if tpe != null
     *
     *  Attempting to set the symbol of a Tree which does not support
     *  it will induce an exception.
     */
    val symbol: Symbol = null
    

    def hasSymbol: Boolean = false
    def isDef: Boolean = false
    def isEmpty: Boolean = false

    def hasSymbolWhich(f: Symbol => Boolean) = hasSymbol && f(symbol)

    /** The canonical way to test if a Tree represents a term.
     */
    def isTerm: Boolean
    
    /** The canonical way to test if a Tree represents a type.
     */
    def isType: Boolean
    
    /** Apply `f` to each subtree */
    def foreach(f: Tree => Unit) { new ForeachTreeTraverser(f).traverse(this) }

    /** Find all subtrees matching predicate `p` */
    def filter(f: Tree => Boolean): List[Tree] = {
      val ft = new FilterTreeTraverser(f)
      ft.traverse(this)
      ft.hits.toList
    }

    /** Returns optionally first tree (in a preorder traversal) which satisfies predicate `p`,
     *  or None if none exists.
     */
    def find(p: Tree => Boolean): Option[Tree] = {
      val ft = new FindTreeTraverser(p)
      ft.traverse(this)
      ft.result
    }

    /** Is there part of this tree which satisfies predicate `p`? */
    def exists(p: Tree => Boolean): Boolean = !find(p).isEmpty

    def equalsStructure(that : Tree) = equalsStructure0(that)(_ eq _)
    def equalsStructure0(that: Tree)(f: (Tree,Tree) => Boolean): Boolean =
      f(this, that) || ((this.productArity == that.productArity) && {
        def equals0(this0: Any, that0: Any): Boolean = (this0, that0) match {
          case (x: Tree, y: Tree)         => f(x, y) || (x equalsStructure0 y)(f)
          case (xs: List[_], ys: List[_]) => (xs corresponds ys)(equals0)
          case _                          => this0 == that0
        }
        def compareOriginals() = (this, that) match {
          case (x: TypeTree, y: TypeTree) if x.original != null && y.original != null =>
            (x.original equalsStructure0 y.original)(f)
          case _                          =>
            true
        }

        (this.productIterator zip that.productIterator forall { case (x, y) => equals0(x, y) }) && compareOriginals()
      })

    /** The direct child trees of this tree.
     *  EmptyTrees are always omitted.  Lists are flattened.
     */
    def children: List[Tree] = {
      def subtrees(x: Any): List[Tree] = x match {
        case EmptyTree   => Nil
        case t: Tree     => List(t)
        case xs: List[_] => xs flatMap subtrees
        case _           => Nil
      }
      productIterator.toList flatMap subtrees
    }

    override def hashCode(): Int = System.identityHashCode(this)
    override def equals(that: Any) = this eq that.asInstanceOf[AnyRef]
  }

  /** A tree for a term.  Not all terms are TermTrees; use isTerm
   *  to reliably identify terms.
   */
  trait TermTree extends Tree

  /** A tree for a type.  Not all types are TypTrees; use isType
   *  to reliably identify types.
   */
  trait TypTree extends Tree

  /** A tree with a mutable symbol field, initialized to NoSymbol.
   */
  trait SymTree extends Tree {
    override def hasSymbol = true
    override var symbol: Symbol = NoSymbol
  }

  /** A tree which references a symbol-carrying entity.
   *  References one, as opposed to defining one; definitions
   *  are in DefTrees.
   */
  trait RefTree extends SymTree {
    def name: Name
  }

  /** A tree which defines a symbol-carrying entity.
   */
  abstract class DefTree extends SymTree {
    def name: Name
    override def isDef = true
  }

// ----- tree node alternatives --------------------------------------

  /** The empty tree */
  case object EmptyTree extends TermTree {
    override def isEmpty = true
  }

  /** Common base class for all member definitions: types, classes,
   *  objects, packages, vals and vars, defs.
   */
  abstract class MemberDef extends DefTree {
    def mods: Modifiers
    def keyword: String = this match {
      case TypeDef(_, _, _, _)      => "type"
      case ClassDef(mods, _, _, _)  => if (mods hasModifier Modifier.`trait`) "trait" else "class"
      case DefDef(_, _, _, _, _, _) => "def"
      case ModuleDef(_, _, _)       => "object"
      case PackageDef(_, _)         => "package"
      case ValDef(mods, _, _, _)    => if (mods hasModifier Modifier.mutable) "var" else "val"
      case _ => ""
    }
    // final def hasFlag(mask: Long): Boolean = mods hasFlag mask
  }

  /** A packaging, such as `package pid { stats }`
   */
  case class PackageDef(pid: RefTree, stats: List[Tree])(pos: Position, sym: Symbol, tpe: Type)
       extends MemberDef {
    def name = pid.name
    def mods = Modifiers()
  }

  /** A common base class for class and object definitions.
   */
  abstract class ImplDef extends MemberDef {
    def impl: Template
  }

  /** A class definition.
   */
  case class ClassDef(mods: Modifiers, name: TypeName, tparams: List[TypeDef], impl: Template)(pos: Position, sym: Symbol, tpe: Type)
       extends ImplDef

  /** An object definition, e.g. `object Foo`.  Internally, objects are
   *  quite frequently called modules to reduce ambiguity.
   */
  case class ModuleDef(mods: Modifiers, name: TermName, impl: Template)(pos: Position, sym: Symbol, tpe: Type)
       extends ImplDef

  /** A common base class for ValDefs and DefDefs.
   */
  abstract class ValOrDefDef extends MemberDef {
    def name: Name // can't be a TermName because macros can be type names.
    def tpt: Tree
    def rhs: Tree
  }

  /** A value definition (this includes vars as well, which differ from
   *  vals only in having the MUTABLE flag set in their Modifiers.)
   */
  case class ValDef(mods: Modifiers, name: TermName, tpt: Tree, rhs: Tree)(pos: Position, sym: Symbol, tpe: Type) extends ValOrDefDef

  /** A method or macro definition.
   *  @param name   The name of the method or macro. Can be a type name in case this is a type macro
   */
  case class DefDef(mods: Modifiers, name: Name, tparams: List[TypeDef],
                    vparamss: List[List[ValDef]], tpt: Tree, rhs: Tree)(pos: Position, sym: Symbol, tpe: Type) extends ValOrDefDef

  /** An abstract type, a type parameter, or a type alias.
   */
  case class TypeDef(mods: Modifiers, name: TypeName, tparams: List[TypeDef], rhs: Tree)(pos: Position, sym: Symbol, tpe: Type)
       extends MemberDef

  /** A labelled expression.  Not expressible in language syntax, but
   *  generated by the compiler to simulate while/do-while loops, and
   *  also by the pattern matcher.
   *
   *  The label acts much like a nested function, where `params` represents
   *  the incoming parameters.  The symbol given to the LabelDef should have
   *  a MethodType, as if it were a nested function.
   *
   *  Jumps are apply nodes attributed with a label's symbol.  The
   *  arguments from the apply node will be passed to the label and
   *  assigned to the Idents.
   *
   *  Forward jumps within a block are allowed.
   */
  case class LabelDef(name: TermName, params: List[Ident], rhs: Tree)(pos: Position, sym: Symbol, tpe: Type)
       extends DefTree with TermTree

  /** Import selector
   *
   * Representation of an imported name its optional rename and their optional positions
   *
   * @param name      the imported name
   * @param namePos   its position or -1 if undefined
   * @param rename    the name the import is renamed to (== name if no renaming)
   * @param renamePos the position of the rename or -1 if undefined
   */
  case class ImportSelector(name: Name, namePos: Int, rename: Name, renamePos: Int)(pos: Position, sym: Symbol, tpe: Type)

  /** Import clause
   *
   *  @param expr
   *  @param selectors
   */
  case class Import(expr: Tree, selectors: List[ImportSelector])(pos: Position, sym: Symbol, tpe: Type)
       extends SymTree
    // The symbol of an Import is an import symbol @see Symbol.newImport
    // It's used primarily as a marker to check that the import has been typechecked.

  /** Instantiation template of a class or trait
   *
   *  @param parents
   *  @param body
   */
  case class Template(parents: List[Tree], self: ValDef, body: List[Tree])(pos: Position, sym: Symbol, tpe: Type)
       extends SymTree {
    // the symbol of a template is a local dummy. @see Symbol.newLocalDummy
    // the owner of the local dummy is the enclosing trait or class.
    // the local dummy is itself the owner of any local blocks
    // For example:
    //
    // class C {
    //   def foo // owner is C
    //   {
    //      def bar  // owner is local dummy
    //   }
    // System.err.println("TEMPLATE: " + parents)
  }

  /** Block of expressions (semicolon separated expressions) */
  case class Block(stats: List[Tree], expr: Tree)(pos: Position, sym: Symbol, tpe: Type)
       extends TermTree

  /** Case clause in a pattern match, eliminated during explicitouter
   *  (except for occurrences in switch statements)
   */
  case class CaseDef(pat: Tree, guard: Tree, body: Tree)(pos: Position, sym: Symbol, tpe: Type)
       extends Tree

  /** Alternatives of patterns, eliminated by explicitouter, except for
   *  occurrences in encoded Switch stmt (=remaining Match(CaseDef(...))
   */
  case class Alternative(trees: List[Tree])(pos: Position, sym: Symbol, tpe: Type)
       extends TermTree

  /** Repetition of pattern, eliminated by explicitouter */
  case class Star(elem: Tree)(pos: Position, sym: Symbol, tpe: Type)
       extends TermTree

  /** Bind of a variable to a rhs pattern, eliminated by explicitouter
   *
   *  @param name
   *  @param body
   */
  case class Bind(name: Name, body: Tree)(pos: Position, sym: Symbol, tpe: Type)
       extends DefTree

  case class UnApply(fun: Tree, args: List[Tree])(pos: Position, sym: Symbol, tpe: Type)
       extends TermTree

  /** Array of expressions, needs to be translated in backend,
   */
  case class ArrayValue(elemtpt: Tree, elems: List[Tree])(pos: Position, sym: Symbol, tpe: Type)
       extends TermTree

  /** Anonymous function, eliminated by analyzer */
  case class Function(vparams: List[ValDef], body: Tree)(pos: Position, sym: Symbol, tpe: Type)
       extends TermTree with SymTree
    // The symbol of a Function is a synthetic value of name nme.ANON_FUN_NAME
    // It is the owner of the function's parameters.

  /** Assignment */
  case class Assign(lhs: Tree, rhs: Tree)(pos: Position, sym: Symbol, tpe: Type)
       extends TermTree

  /** Either an assignment or a named argument. Only appears in argument lists,
   *  eliminated by typecheck (doTypedApply)
   */
  case class AssignOrNamedArg(lhs: Tree, rhs: Tree)(pos: Position, sym: Symbol, tpe: Type)
       extends TermTree

  /** Conditional expression */
  case class If(cond: Tree, thenp: Tree, elsep: Tree)(pos: Position, sym: Symbol, tpe: Type)
       extends TermTree

  /** - Pattern matching expression  (before explicitouter)
   *  - Switch statements            (after explicitouter)
   *
   *  After explicitouter, cases will satisfy the following constraints:
   *
   *  - all guards are `EmptyTree`,
   *  - all patterns will be either `Literal(Constant(x:Int))`
   *    or `Alternative(lit|...|lit)`
   *  - except for an "otherwise" branch, which has pattern
   *    `Ident(nme.WILDCARD)`
   */
  case class Match(selector: Tree, cases: List[CaseDef])(pos: Position, sym: Symbol, tpe: Type)
       extends TermTree

  /** Return expression */
  case class Return(expr: Tree)(pos: Position, sym: Symbol, tpe: Type)
       extends TermTree with SymTree
    // The symbol of a Return node is the enclosing method.

  case class Try(block: Tree, catches: List[CaseDef], finalizer: Tree)(pos: Position, sym: Symbol, tpe: Type)
       extends TermTree

  /** Throw expression */
  case class Throw(expr: Tree)(pos: Position, sym: Symbol, tpe: Type)
       extends TermTree

  /** Object instantiation
   *  One should always use factory method below to build a user level new.
   *
   *  @param tpt    a class type
   */
  case class New(tpt: Tree)(pos: Position, sym: Symbol, tpe: Type) extends TermTree

  /** Type annotation, eliminated by explicit outer */
  case class Typed(expr: Tree, tpt: Tree)(pos: Position, sym: Symbol, tpe: Type)
       extends TermTree

  /** Common base class for Apply and TypeApply. This could in principle
   *  be a SymTree, but whether or not a Tree is a SymTree isn't used
   *  to settle any interesting questions, and it would add a useless
   *  field to all the instances (useless, since GenericApply forwards to
   *  the underlying fun.)
   */
  abstract class GenericApply extends TermTree {
    val fun: Tree
    val args: List[Tree]
  }

  /** Explicit type application.
   *  @PP: All signs point toward it being a requirement that args.nonEmpty,
   *  but I can't find that explicitly stated anywhere.  Unless your last name
   *  is odersky, you should probably treat it as true.
   */
  case class TypeApply(fun: Tree, args: List[Tree])(pos: Position, sym: Symbol, tpe: Type)
       extends GenericApply {
    override def symbol: Symbol = fun.symbol
  }

  /** Value application */
  case class Apply(fun: Tree, args: List[Tree])(pos: Position, sym: Symbol, tpe: Type)
       extends GenericApply {
    override def symbol: Symbol = fun.symbol
  }

  class ApplyToImplicitArgs(fun: Tree, args: List[Tree])(pos: Position, sym: Symbol, tpe: Type) extends Apply(fun, args)(pos, sym, tpe)

  class ApplyImplicitView(fun: Tree, args: List[Tree])(pos: Position, sym: Symbol, tpe: Type) extends Apply(fun, args)(pos, sym, tpe)

  /** Dynamic value application.
   *  In a dynamic application   q.f(as)
   *   - q is stored in qual
   *   - as is stored in args
   *   - f is stored as the node's symbol field.
   */
  case class ApplyDynamic(qual: Tree, args: List[Tree])(pos: Position, sym: Symbol, tpe: Type)
       extends TermTree with SymTree
    // The symbol of an ApplyDynamic is the function symbol of `qual`, or NoSymbol, if there is none.

  /** Super reference, qual = corresponding this reference */
  case class Super(qual: Tree, mix: TypeName)(pos: Position, sym: Symbol, tpe: Type) extends TermTree {
    
    /** The symbol of a Super is the class _from_ which the super reference is made.
     *  For instance in C.super(...), it would be C. */
    override def symbol: Symbol = qual.symbol
  }

  /** Self reference */
  case class This(qual: TypeName)(pos: Position, sym: Symbol, tpe: Type)
        extends TermTree with SymTree
    // The symbol of a This is the class to which the this refers.
    // For instance in C.this, it would be C.

  /** Designator <qualifier> . <name> */
  case class Select(qualifier: Tree, name: Name)(pos: Position, sym: Symbol, tpe: Type)
       extends RefTree

  /** Identifier <name> */
  case class Ident(name: Name)(pos: Position, sym: Symbol, tpe: Type) extends RefTree

  class BackQuotedIdent(name: Name)(pos: Position, sym: Symbol, tpe: Type) extends Ident(name)(pos, sym, tpe)

  /** Literal */
  case class Literal(value: Constant)(pos: Position, sym: Symbol, tpe: Type)
        extends TermTree {
    assert(value ne null)
  }

  /** A tree that has an annotation attached to it. Only used for annotated types and
   *  annotation ascriptions, annotations on definitions are stored in the Modifiers.
   *  Eliminated by typechecker (typedAnnotated), the annotations are then stored in
   *  an AnnotatedType.
   */
  case class Annotated(annot: Tree, arg: Tree)(pos: Position, sym: Symbol, tpe: Type) extends Tree

  /** Singleton type, eliminated by RefCheck */
  case class SingletonTypeTree(ref: Tree)(pos: Position, sym: Symbol, tpe: Type)
        extends TypTree

  /** Type selection <qualifier> # <name>, eliminated by RefCheck */
  case class SelectFromTypeTree(qualifier: Tree, name: TypeName)(pos: Position, sym: Symbol, tpe: Type)
       extends TypTree with RefTree

  /** Intersection type <parent1> with ... with <parentN> { <decls> }, eliminated by RefCheck */
  case class CompoundTypeTree(templ: Template)(pos: Position, sym: Symbol, tpe: Type)
       extends TypTree

  /** Applied type <tpt> [ <args> ], eliminated by RefCheck */
  case class AppliedTypeTree(tpt: Tree, args: List[Tree])(pos: Position, sym: Symbol, tpe: Type)
       extends TypTree {
    override def symbol: Symbol = tpt.symbol
  }

  case class TypeBoundsTree(lo: Tree, hi: Tree)(pos: Position, sym: Symbol, tpe: Type)
       extends TypTree

  case class ExistentialTypeTree(tpt: Tree, whereClauses: List[Tree])(pos: Position, sym: Symbol, tpe: Type)
       extends TypTree

  /** A synthetic tree holding an arbitrary type.  Not to be confused with
    * with TypTree, the trait for trees that are only used for type trees.
    * TypeTree's are inserted in several places, but most notably in
    * `RefCheck`, where the arbitrary type trees are all replaced by
    * TypeTree's. */
  case class TypeTree()(pos: Position, sym: Symbol, tpe: Type, orig: Tree) extends TypTree {
    override def symbol = if (tpe == null) null else tpe.typeSymbol
    override def isEmpty = (tpe eq null) || tpe == NoType

    def original: Tree = orig
  }

  /** An empty deferred value definition corresponding to:
   *    val _: _
   *  This is used as a placeholder in the `self` parameter Template if there is
   *  no definition of a self value of self type.
   */
  def emptyValDef: ValDef

  // ------ traversers, copiers, and transformers ---------------------------------------------

  class Traverser {
    protected var currentOwner: Symbol = definitions.RootClass

    def traverse(tree: Tree): Unit = tree match {
      case EmptyTree =>
        ;
      case PackageDef(pid, stats) =>
        traverse(pid)
        atOwner(tree.symbol.moduleClass) {
          traverseTrees(stats)
        }
      case ClassDef(mods, name, tparams, impl) =>
        atOwner(tree.symbol) {
          traverseTrees(mods.annotations); traverseTrees(tparams); traverse(impl)
        }
      case ModuleDef(mods, name, impl) =>
        atOwner(tree.symbol.moduleClass) {
          traverseTrees(mods.annotations); traverse(impl)
        }
      case ValDef(mods, name, tpt, rhs) =>
        atOwner(tree.symbol) {
          traverseTrees(mods.annotations); traverse(tpt); traverse(rhs)
        }
      case DefDef(mods, name, tparams, vparamss, tpt, rhs) =>
        atOwner(tree.symbol) {
          traverseTrees(mods.annotations); traverseTrees(tparams); traverseTreess(vparamss); traverse(tpt); traverse(rhs)
        }
      case TypeDef(mods, name, tparams, rhs) =>
        atOwner(tree.symbol) {
          traverseTrees(mods.annotations); traverseTrees(tparams); traverse(rhs)
        }
      case LabelDef(name, params, rhs) =>
        traverseTrees(params); traverse(rhs)
      case Import(expr, selectors) =>
        traverse(expr)
      case Annotated(annot, arg) =>
        traverse(annot); traverse(arg)
      case Template(parents, self, body) =>
        traverseTrees(parents)
        if (!self.isEmpty) traverse(self)
        traverseStats(body, tree.symbol)
      case Block(stats, expr) =>
        traverseTrees(stats); traverse(expr)
      case CaseDef(pat, guard, body) =>
        traverse(pat); traverse(guard); traverse(body)
      case Alternative(trees) =>
        traverseTrees(trees)
      case Star(elem) =>
        traverse(elem)
      case Bind(name, body) =>
        traverse(body)
      case UnApply(fun, args) =>
        traverse(fun); traverseTrees(args)
      case ArrayValue(elemtpt, trees) =>
        traverse(elemtpt); traverseTrees(trees)
      case Function(vparams, body) =>
        atOwner(tree.symbol) {
          traverseTrees(vparams); traverse(body)
        }
      case Assign(lhs, rhs) =>
        traverse(lhs); traverse(rhs)
      case AssignOrNamedArg(lhs, rhs) =>
        traverse(lhs); traverse(rhs)
      case If(cond, thenp, elsep) =>
        traverse(cond); traverse(thenp); traverse(elsep)
      case Match(selector, cases) =>
        traverse(selector); traverseTrees(cases)
      case Return(expr) =>
        traverse(expr)
      case Try(block, catches, finalizer) =>
        traverse(block); traverseTrees(catches); traverse(finalizer)
      case Throw(expr) =>
        traverse(expr)
      case New(tpt) =>
        traverse(tpt)
      case Typed(expr, tpt) =>
        traverse(expr); traverse(tpt)
      case TypeApply(fun, args) =>
        traverse(fun); traverseTrees(args)
      case Apply(fun, args) =>
        traverse(fun); traverseTrees(args)
      case ApplyDynamic(qual, args) =>
        traverse(qual); traverseTrees(args)
      case Super(qual, _) =>
        traverse(qual)
      case This(_) =>
        ;
      case Select(qualifier, selector) =>
        traverse(qualifier)
      case Ident(_) =>
        ;
      case Literal(_) =>
        ;
      case TypeTree() =>
        ;
      case SingletonTypeTree(ref) =>
        traverse(ref)
      case SelectFromTypeTree(qualifier, selector) =>
        traverse(qualifier)
      case CompoundTypeTree(templ) =>
        traverse(templ)
      case AppliedTypeTree(tpt, args) =>
        traverse(tpt); traverseTrees(args)
      case TypeBoundsTree(lo, hi) =>
        traverse(lo); traverse(hi)
      case ExistentialTypeTree(tpt, whereClauses) =>
        traverse(tpt); traverseTrees(whereClauses)
      case _ => xtraverse(this, tree)
    }

    def traverseTrees(trees: List[Tree]) {
      trees foreach traverse
    }
    def traverseTreess(treess: List[List[Tree]]) {
      treess foreach traverseTrees
    }
    def traverseStats(stats: List[Tree], exprOwner: Symbol) {
      stats foreach (stat =>
        if (exprOwner != currentOwner) atOwner(exprOwner)(traverse(stat))
        else traverse(stat)
      )
    }

    def atOwner(owner: Symbol)(traverse: => Unit) {
      val prevOwner = currentOwner
      currentOwner = owner
      traverse
      currentOwner = prevOwner
    }

    /** Leave apply available in the generic traverser to do something else.
     */
    def apply[T <: Tree](tree: T): T = { traverse(tree); tree }
  }

  protected def xtraverse(traverser: Traverser, tree: Tree): Unit = throw new MatchError(tree)

  class ForeachTreeTraverser(f: Tree => Unit) extends Traverser {
    override def traverse(t: Tree) {
      f(t)
      super.traverse(t)
    }
  }

  class FilterTreeTraverser(p: Tree => Boolean) extends Traverser {
    val hits = new ListBuffer[Tree]
    override def traverse(t: Tree) {
      if (p(t)) hits += t
      super.traverse(t)
    }
  }

  class FindTreeTraverser(p: Tree => Boolean) extends Traverser {
    var result: Option[Tree] = None
    override def traverse(t: Tree) {
      if (result.isEmpty) {
        if (p(t)) result = Some(t)
        super.traverse(t)
      }
    }
  }

/* A standard pattern match
  case EmptyTree =>
  case PackageDef(pid, stats) =>
     // package pid { stats }
  case ClassDef(mods, name, tparams, impl) =>
     // mods class name [tparams] impl   where impl = extends parents { defs }
  case ModuleDef(mods, name, impl) =>                             (eliminated by refcheck)
     // mods object name impl  where impl = extends parents { defs }
  case ValDef(mods, name, tpt, rhs) =>
     // mods val name: tpt = rhs
     // note missing type information is expressed by tpt = TypeTree()
  case DefDef(mods, name, tparams, vparamss, tpt, rhs) =>
     // mods def name[tparams](vparams_1)...(vparams_n): tpt = rhs
     // note missing type information is expressed by tpt = TypeTree()
  case TypeDef(mods, name, tparams, rhs) =>                       (eliminated by erasure)
     // mods type name[tparams] = rhs
     // mods type name[tparams] >: lo <: hi,  where lo, hi are in a TypeBoundsTree,
                                              and DEFERRED is set in mods
  case LabelDef(name, params, rhs) =>
     // used for tailcalls and like
     // while/do are desugared to label defs as follows:
     // while (cond) body ==> LabelDef($L, List(), if (cond) { body; L$() } else ())
     // do body while (cond) ==> LabelDef($L, List(), body; if (cond) L$() else ())
  case Import(expr, selectors) =>                                 (eliminated by typecheck)
     // import expr.{selectors}
     // Selectors are a list of pairs of names (from, to).
     // The last (and maybe only name) may be a nme.WILDCARD
     // for instance
     //   import qual.{x, y => z, _}  would be represented as
     //   Import(qual, List(("x", "x"), ("y", "z"), (WILDCARD, null)))
  case Template(parents, self, body) =>
     // extends parents { self => body }
     // if self is missing it is represented as emptyValDef
  case Block(stats, expr) =>
     // { stats; expr }
  case CaseDef(pat, guard, body) =>                               (eliminated by transmatch/explicitouter)
    // case pat if guard => body
  case Alternative(trees) =>                                      (eliminated by transmatch/explicitouter)
    // pat1 | ... | patn
  case Star(elem) =>                                              (eliminated by transmatch/explicitouter)
    // pat*
  case Bind(name, body) =>                                        (eliminated by transmatch/explicitouter)
    // name @ pat
  case UnApply(fun: Tree, args)                                   (introduced by typer, eliminated by transmatch/explicitouter)
    // used for unapply's
  case ArrayValue(elemtpt, trees) =>                              (introduced by uncurry)
    // used to pass arguments to vararg arguments
    // for instance, printf("%s%d", foo, 42) is translated to after uncurry to:
    // Apply(
    //   Ident("printf"),
    //   Literal("%s%d"),
    //   ArrayValue(<Any>, List(Ident("foo"), Literal(42))))
  case Function(vparams, body) =>                                 (eliminated by lambdaLift)
    // vparams => body  where vparams:List[ValDef]
  case Assign(lhs, rhs) =>
    // lhs = rhs
  case AssignOrNamedArg(lhs, rhs) =>                              (eliminated by typer, resurrected by reifier)
    // @annotation(lhs = rhs)
  case If(cond, thenp, elsep) =>
    // if (cond) thenp else elsep
  case Match(selector, cases) =>
    // selector match { cases }
  case Return(expr) =>
    // return expr
  case Try(block, catches, finalizer) =>
    // try block catch { catches } finally finalizer where catches: List[CaseDef]
  case Throw(expr) =>
    // throw expr
  case New(tpt) =>
    // new tpt   always in the context: (new tpt).<init>[targs](args)
  case Typed(expr, tpt) =>                                        (eliminated by erasure)
    // expr: tpt
  case TypeApply(fun, args) =>
    // fun[args]
  case Apply(fun, args) =>
    // fun(args)
    // for instance fun[targs](args)  is expressed as  Apply(TypeApply(fun, targs), args)
  case ApplyDynamic(qual, args)                                   (introduced by erasure, eliminated by cleanup)
    // fun(args)
  case Super(qual, mix) =>
    // qual.super[mix]     if qual and/or mix is empty, ther are tpnme.EMPTY
  case This(qual) =>
    // qual.this
  case Select(qualifier, selector) =>
    // qualifier.selector
  case Ident(name) =>
    // name
    // note: type checker converts idents that refer to enclosing fields or methods
    // to selects; name ==> this.name
  case Literal(value) =>
    // value
  case TypeTree() =>                                              (introduced by refcheck)
    // a type that's not written out, but given in the tpe attribute
  case Annotated(annot, arg) =>                                   (eliminated by typer)
    // arg @annot  for types,  arg: @annot for exprs
  case SingletonTypeTree(ref) =>                                  (eliminated by uncurry)
    // ref.type
  case SelectFromTypeTree(qualifier, selector) =>                 (eliminated by uncurry)
    // qualifier # selector, a path-dependent type p.T is expressed as p.type # T
  case CompoundTypeTree(templ: Template) =>                       (eliminated by uncurry)
    // parent1 with ... with parentN { refinement }
  case AppliedTypeTree(tpt, args) =>                              (eliminated by uncurry)
    // tpt[args]
  case TypeBoundsTree(lo, hi) =>                                  (eliminated by uncurry)
    // >: lo <: hi
  case ExistentialTypeTree(tpt, whereClauses) =>                  (eliminated by uncurry)
    // tpt forSome { whereClauses }
*/
}

