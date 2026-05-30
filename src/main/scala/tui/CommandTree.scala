package tui

object CommandTree {

  sealed trait CommandTree[+A] {

    def fold[B](z: B)(f: (CommandTree[A], Seq[B]) => B): B = {
      val childResults: Seq[B] = this match {
        case CommandRoot(children)    => children.map(_.fold(z)(f))
        case CommandNode(_, children) => children.map(_.fold(z)(f))
        case _: CommandLeaf[_]        => Seq.empty
      }
      f(this, childResults)
    }

    def collect[B](pf: PartialFunction[CommandTree[A], B]): Seq[B] = {
      val results = collection.mutable.ListBuffer[B]()
      this.foreach { node =>
        if (pf.isDefinedAt(node)) results += pf(node)
      }
      results.toSeq
    }

    def foreach(f: CommandTree[A] => Unit): Unit = {
      this.fold(()) { (node, _) => f(node) }
    }

    def show: String = {
      fold("") { (node, children) =>
        val name = node match {
          case CommandRoot(_)         => "@Root"
          case CommandNode(cmd, _)    => s"[+] $cmd"
          case CommandLeaf(cmd, _, _) => s"[-] $cmd"
        }
        (name +: children.map(c => "  " + c.replace("\n", "\n  "))).mkString("\n")
      }
    }
  }

  case class CommandRoot[A](child: List[CommandTree[A]]) extends CommandTree[A] {
    def getCollision() = getCollisionError(this)
  }

  case class CommandNode[A](cmd: String, child: Seq[CommandTree[A]]) extends CommandTree[A]

  case class CommandLeaf[A](
                             cmd: String,
                             alias: Seq[String] = Seq.empty,
                             handler: Seq[String] => Either[Exception,A] = (_: Seq[String]) => Left(new Exception("Not-Defined"))
                           ) extends CommandTree[A]

  object CommandRoot {
    def apply[A](child: CommandTree[A]*): CommandRoot[A] = new CommandRoot(child.toList)
  }

  def execute[A](tree: CommandTree[A], input: Seq[String]): Either[Exception, A] = {
    def findAndRun(current: CommandTree[A], args: Seq[String]): Either[Exception, A] = {
      if (args.isEmpty) return Left(new NoSuchElementException("No input provided"))

      val head = args.head
      val tail = args.tail

      val children = current match {
        case CommandRoot(child)    => child
        case CommandNode(_, child) => child
        case _                     => Seq.empty
      }

      children.find {
        case l: CommandLeaf[A] => l.cmd == head || l.alias.contains(head)
        case n: CommandNode[A] => n.cmd == head
        case _                 => false
      } match {
        case Some(leaf: CommandLeaf[A]) => leaf.handler(tail)
        case Some(node: CommandNode[A]) => findAndRun(node, tail)
        case None => Left(new NoSuchElementException(s"Unknown command: $head"))
      }
    }
    findAndRun(tree, input)
  }

  case class CollisionError(path: String, duplicatedCmd: String) {
    override def toString: String = s"Colllision( path: $path, duplicated: $duplicatedCmd)"
  }

  def getCollisionError[A](tree: CommandTree[A]): Seq[CollisionError] = {
    def walk(node: CommandTree[A], path: String): Seq[CollisionError] = {
      val children = node match {
        case CommandRoot(child) => child
        case CommandNode(_, child) => child
        case _ => Seq.empty
      }
      if (children.isEmpty) return Seq.empty

      val namesWithNodes = children.flatMap {
        case n@CommandNode(cmd, _) => Seq(cmd -> n)
        case l@CommandLeaf(cmd, alias, _) => (cmd +: alias).map(_ -> l)
        case _ => Seq.empty
      }

      val collisions = namesWithNodes.groupBy(_._1).filter(_._2.size > 1).map {
        case (name, _) => CollisionError(path, name)
      }.toSeq

      val childCollisions = children.flatMap {
        case n: CommandNode[A] => walk(n, s"$path/${n.cmd}")
        case l: CommandLeaf[A] => walk(l, s"$path/${l.cmd}")
        case _ => Seq.empty
      }
      collisions ++ childCollisions
    }
    walk(tree, "root")
  }

  // --- DSL Implicits ---
  // --------------------------------------------------------------------------------
  implicit def stringToLeaf(s: String): CommandLeaf[String] = CommandLeaf[String](s)

  implicit class CommandOps(val name: String) {
    def ~> [A](child: CommandTree[A]*): CommandTree[A] = CommandNode(name, child)
    def unary_~ : CommandLeaf[String] = CommandLeaf[String](name)
    def | (alias: String): CommandLeaf[String] = CommandLeaf[String](name, Seq(alias))

    def >> (h: => Either[Exception,String]): CommandLeaf[String] = CommandLeaf[String](name, handler = _ => h)
    def >>> [B](h: Seq[String] => Either[Exception,B]): CommandLeaf[B] = CommandLeaf[B](name, handler =  h)
  }

  implicit class TreeOps[A](val tree: CommandTree[A]) {
    def ++(next: CommandTree[A]): Seq[CommandTree[A]] = Seq(tree, next)
  }

  implicit class SeqOps[A](val trees: Seq[CommandTree[A]]) {
    def ++(next: CommandTree[A]): Seq[CommandTree[A]] = trees :+ next
  }

  implicit class LeafOps[A](val leaf: CommandLeaf[A]) {
    def | (alias: String): CommandLeaf[A] = leaf.copy(alias = leaf.alias :+ alias)
    def >> (h: Seq[String] => Either[Exception,String]): CommandLeaf[String] = CommandLeaf[String](leaf.cmd, leaf.alias, h)
    def >> (h: => Either[Exception,String]): CommandLeaf[String] = CommandLeaf[String](leaf.cmd, leaf.alias, _ => h)
    def >>> [B](h: Seq[String] => Either[Exception,B]): CommandLeaf[B] = CommandLeaf[B](leaf.cmd, leaf.alias, h)
  }

  def go(args: Array[String]) = {
    specTest ()
  }

  def specTest() = {

    val myTree = CommandRoot[String](
      ("status" | "st") >> {
        Right("System is running")
      },
      "echo" >>> { args =>
        Either.cond( args.nonEmpty,
          args.mkString(" "),
          new Exception("isEmpty")
        )
      }
    )

    val result = execute(myTree, Seq("echo", "this", "is", "good"))
    result.foreach(msg => println(s"결과: $msg"))

    val myCLI = CommandRoot[String](
      ("status" | "st" | "s") >> Right("Checking status..."),
     "git" ~> (
        ("commit" | "ci") >> Right("Committing..."),
        (~"push") >> Right("Pushing to origin...")
      ),
      "docker" ~> (
        "image" ~> (
          "ls" >> Right("Listing images..."),
          "prune" >> Right("Cleaning up...")
        ),
        "ps" >> Right("Listing containers...")
      ),
      "exit" >> Right("Goodbye!")
    )

    println(myCLI.show)


    val problematicTree = CommandRoot[String](

      // status
      "status" >> Right("System Normal"),
      "status" >> Right("System Overloaded"),

      // g
      ("git" | "g") >> Right("Git Helper"),

      "g" >> Right("Global Shortcut"), // 'g'가 위 노드의 별칭과 충돌

      // go
      ("start" | "run" | "go") >> Right("Starting service..."),
      ("execute" | "go") >> Right("Executing task..."), // 'go'가 중복됨

      // l
      "docker" ~> (
        ("ps" | "l") >> Right("List containers"),
        ("images" | "l") >> Right("List images") // 부모(docker) 아래에서 'l'이 중복됨
      ),

      // ok
      "network" ~> ( "status" >> Right("Network is UP") )
    )

    val err = problematicTree.getCollision()

    err.foreach(println)

  }
}

