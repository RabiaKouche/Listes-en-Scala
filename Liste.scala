
case class Queue[T](in:List[T]=Nil ,out:List[T]=Nil) {


/** Ajoute un élément `x` en tête. */
def enqueue(X:T):Queue[T] = Queue(X::this.in,this.out)


/** Retire le dernier élement */
def dequeue():(Option[T],Queue[T]) = out match {
    
    case x::y => (Option(x), Queue(in, y))
    case Nil => in.reverse match {
    case x :: y => (Option(x), Queue(Nil, y))
    case Nil => (None, Queue(Nil, Nil))
      
    }
  }

/** Accès au premier élément, s'il existe (dernier élément entré). */
def headOption():Option[T] = 

  if( this.isEmpty)  None

  else if((!this.in.isEmpty) && (this.out.isEmpty)) Some(this.in.head)

  else Some(this.out.reverse.head)


/** Vrai si la liste est vide faux sinon. */
def isEmpty:Boolean = in.isEmpty && out.isEmpty

/** retourner la taille de la queue */
def length():Int = this.in.length+this.out.length


/** retourner le dernier élément de la queue */
def rearOption():Option[T] = this match {
      
  case Queue((Nil),(Nil)) => None
  case Queue((x),(Nil))   => Some(x.last)
  case Queue((Nil),(x))   => Some(x.head)
  case Queue((x),(y))     => Some(x.last)
  
  }

/** convertit la Queue en liste simplement chaînée */
def toList():List[T] =

  if (this.isEmpty) Nil

  else if((!this.in.isEmpty) && (this.out.isEmpty)) this.in

  else this.out.reverse ++this.in



/** Effectue la fonction map sur les deux liste */
def map[B](f:T => B):Queue[B] = Queue(in.map(f), out.map(f))

/** Effectue la fonction foldLeft sur les deux liste */
def foldLeft[B](start:B)(f:(B, T) => B):B = toList.foldLeft(start)(f)

/** Ré-implantez isEmpty avec match. return vrai si vide */
def isEmptyMatch():Boolean = this match{
  
  case Queue(Nil, Nil) => true
  case Queue((x::_),(Nil)) => false
  case Queue((Nil),(x::_)) => false
  case Queue((x),(y)) => false
  
  

  }
}


object TestQueue {
  import scala.io.AnsiColor._

 
  def Test[A](text:String, test:A, result:A): Boolean = {
    val check:Boolean = test.equals(result)
    println("\t" + text + (if (check) s"${GREEN}✔${RESET}" else s"${RED}X${RESET}" + "\n\t\tReceived : " + test + "\n\t\tExpected : " + result))
    check
  }

  def main(args: Array[String]): Unit = {
    println("\n Testes unitaires : test passé (✔), test n'est pas passé (X)")

        
    println("\n Test sur enqueue() ")

    Test("Ajouter un element dans une Queue vide : ",
      Queue[Int](Nil,Nil).enqueue(1),
      Queue[Int](1::Nil,Nil))

    Test("Ajouter un element dans une Queue avec deux elements (in): ",
      Queue[Int](1::2::Nil,Nil).enqueue(0),
      Queue[Int](0::1::2::Nil,Nil))

    Test("Ajout d\'un element dans une Queue avec deux élements (out): ",
      Queue[Int](Nil,1::2::Nil).enqueue(0),
      Queue[Int](0::Nil,1::2::Nil))

    
    println("\n Test sur dequeue() ")

    Test("Retrait un element dans le cas ou la queue est vide : ",
      Queue[Int](Nil,Nil).dequeue(),
      (None, Queue[Int](Nil,Nil)))

    Test("Retrait de dernier élement quand la queue n'est pas vide  (in) : ",
      Queue[Int](1::2::3::Nil,Nil).dequeue(),
      (Some(3), Queue[Int](Nil,2::1::Nil)))

    Test("Retrait de dernier élement quand la queue n'est pas vide (out) : ",
      Queue[Int](Nil,2::3::Nil).dequeue(),
      (Some(2), Queue[Int](Nil,3::Nil)))
    
    
    println("\n Test sur isEmpty ")

    Test("Test avec une Queue vide : ",
      Queue[Int](Nil,Nil).isEmpty,
      true)
    Test("Test avec une Queue qui n'est pas vide : ",
      Queue[Int](1::2::3::Nil,Nil).isEmpty,
      false)
    
    
    
    println("\n Test sur length ")

    Test("Test avec une Queue de taille 0 : ",
      Queue[Int](Nil,Nil).length,
      0)

    Test("Test avec une Queue avec plusieurs élements (in) : ",
      Queue[Int](3::2::1::Nil,Nil).length,
      3)

    Test("Test avec une Queue avec plusieurs élements (out) : ",
      Queue[Int](Nil,2::3::Nil).length,
      2)

    
    
    println("\n Test sur rearOption() ")

    Test("Retourner le premier élement entré dans une Queue vide : ",
      Queue[Int](Nil,Nil).rearOption(),
      None)

    Test("Retourner le premier élement entré dans une Queue avec des element seulement (in) : ",
      Queue[Int](3::2::1::Nil,Nil).rearOption(),
      Some(1))

    Test("Retourner le premier élement entré dans une Queue avec des element seulement  (out) : ",
      Queue[Int](Nil,2::3::Nil).rearOption(),
      Some(2))
    
    
        
    println("\n Test sur toList ")

    Test("Retourner sous forme de liste les élément entré dans une Queue vide : ",
      Queue[Int](Nil,Nil).toList,
      Nil)

    Test("Retourner sous forme d une liste les élements entré dans une Queue avec des elements dans in : ",
      Queue[Int](3::2::1::Nil,Nil).toList,
      3::2::1::Nil)

    Test("Retourner sous forme d une liste les élements entré dans une Queue avec des elements dans out : ",
      Queue[Int](Nil,2::3::Nil).toList,
      3::2::Nil)
    
    
    println("\n Test sur map()")

    Test("Avec une Queue vide : ",
      Queue[Int](Nil,Nil).map(a => "val : " + a),
      Queue[String](Nil,Nil))

    Test("Avec une Queue avec des elements dans in : ",
      Queue[Int](3::2::1::Nil,Nil).map(a => "val : " + a),
      Queue[String]("val : 3"::"val : 2"::"val : 1"::Nil,Nil))

    Test("Avec une Queue avec des elements dans out : ",
      Queue[Int](Nil,2::3::Nil).map(a => "val : " + a),
      Queue[String](Nil,"val : 2"::"val : 3"::Nil))

    
    println("\n Test sur foldLeft()")

    Test("Test avec une Queue vide : ",
      Queue[Int](Nil,Nil).foldLeft(0)((acc, current) => acc + current),
      0)

    Test("Test avec une Queue avec des élements dans in : ",
      Queue[Int](10::5::3::Nil,Nil).foldLeft(0)((acc, current) => acc + current),
      18)

    Test("Test avec une Queue avec des élements seulement dans out : ",
      Queue[Int](Nil,4::2::Nil).foldLeft(0)((acc, current) => acc + current),
      6)

    
    println("\n Test sur isEmptyMatch ")

    Test("Test avec une Queue avec des elements dans in : ",
      Queue[Int](3::2::1::Nil,Nil).isEmptyMatch,
      false)

    Test("Test avec une Queue avec des elements dans out : ",
      Queue[Int](Nil,2::3::Nil).isEmptyMatch,
      false)
    
     Test("Test avec une Queue vide : ",
      Queue[Int](Nil,Nil).isEmptyMatch,
      true)
    
   
    println("\n Test sur headOption() ")

    Test("Retourner le dernier élement entré dans une Queue vide : ",
      Queue[Int](Nil,Nil).headOption(),
      None)

    Test("Retourner le dernier élément entré dans une Queue avec des élements dans in : ",
      Queue[Int](3::2::1::Nil,Nil).headOption(),
      Some(3))

    Test("Retourner le dernier élement entré dans une Queue avec des élements dans out : ",
      Queue[Int](Nil,7::15::Nil).headOption(),
      Some(15)) 

  }
}

