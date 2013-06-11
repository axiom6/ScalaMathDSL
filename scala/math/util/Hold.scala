
package math.util

// ------------------------------- Hode[T] -------------------------------------

class Hode[D]( dataa:D )
{
  type N       = Hode[D]
  var data : D = dataa

  def term            : Hode[D] = Hold.term[D]
  @transient var prev : Hode[D] = term
  @transient var next : Hode[D] = term

  def copy() : Hode[D]  = new Hode[D](data)
  def init() { prev = term; next = term }
  override def toString  : String  = data.toString
}

private class HoldIter[D]( nodea:Hode[D] ) extends Iterator[D]
{
  var node:Hode[D] = nodea
  override def hasNext = node!=null && node!=node.term && node.next!=node.term
  override def next() : D = { val data = node.data; node = node.next; data }
}

// ---------------------------- object Hold[D] ---------------------------------

object Hold
{
  val tern    = new Hode[Nothing]( null.asInstanceOf[Nothing] ) // [Nothing] terminator single
  def term[D] = tern.asInstanceOf[Hode[D]]                      // [Nothing] terminator cast to D
  def apply[D]( seq:Seq[D] ) = new Hold[D]( seq )
  def apply[D]( dat:D )      = new Hold[D]( dat )
  def unapplySeq[D]( seq:Seq[D] ) : Some[Seq[D]] = Some(seq)
}

// ----------------------------- class Hold[D] ---------------------------------

class Hold[D]() // extends Seq[D]
{
//type N = Hode[D]      // Does not work as in the past. Researching an explantion
  var siz : Int   = 0

  @transient val ring:Hode[D] = new Hode[D](null.asInstanceOf[D])
  ring.prev    = ring
  ring.next    = ring

  @transient val term:Hode[D] = Hold.term[D]

  def this( seq:Seq[D] ) =  { this(); for( t <- seq ) add(t) }
  def this( dat:D      ) =  { this(); add(dat) }

  def head : Hode[D] = ring.next
  def tail : Hode[D] = ring.prev
  
  def   lock() {}
  def unlock() {}

  def in( node:Hode[D])  : Boolean = node!=null && node!=ring && node!=term
  def in( idx:Int )      : Boolean = 0 <= idx && idx < siz

// ... Seq ...

  // override def length        : Int         = siz 
  // override def apply( i:Int) : D           = node(i).data


// ... add ins del ...

  private def inc( node:Hode[D] ) { siz += 1 }
  private def dec( node:Hode[D] ) { siz -= 1 }

  // Add to list only if data is unique
  def put( data:D ) : Hode[D] =
  {
    val node:Hode[D] = find(data)
    if(in(node)) node else add(data)
  }
  
// ........ Heap ........

// def put(   key:K, data:D ) : Hode[D]= add(data)
   def upd(  node:Hode[D], data:D ) : Hode[D]= { node.data = data; node }
// def key(  node:Hode[D],  key:K ) : Hode[D]= node
// def del(  node:Hode[D] )         : Hode[D]// Delete data and key from a location
// def node(  key:K )         : Hode[D]= // node(i) 
// def find(  key:K )         : D = // find(To.ID)

   def peekHead : D = head.data // findMin
   def peekTail : D = tail.data // findMax  
   
   def popHead : D =  // deleteMin
   {
      var node : Hode[D]= term
      if( !isEmpty )
          { node = head; del(head) }
      node.data
   }

   def popTail : D =  // deleteMax
   {
      var node : Hode[D]= term
      if( !isEmpty )
          { node = tail; del(tail) }
      node.data
   }   
  
   def size     : Int = siz

   def isEmpty  : Boolean = { siz == 0 }

  def clear()
  {
    lock()
    try
    {
      var node = head
      var next = head
      while( in(node) )
      {
        next = node.next
        del( node )
        node = next
      }
    }
    finally unlock()
  }   
 
 // .......................................

  def create( data:D )  : Hode[D]   = new Hode[D](  data )
  def +=(     data:D )              { add(data) }
  def add(    data:D )  : Hode[D]   = add( create(data) )
  def add( seq:Seq[D] ) : Hode[D]   = { for( data <- seq ) add(data); tail }

  private def add( node:Hode[D] ) : Hode[D]=
  {
    if( !in(node) )
      { Log.error( "node not in", node.toString ); term }

    lock()
    try
    {
      if( isEmpty )        // Add the first node
      {
        node.prev = ring
        node.next = ring
        ring.prev = node
        ring.next = node
      }
      else // Add node to the tail, ring.prev() is the tail
      {
        node.prev      = ring.prev
        node.next      = ring
        ring.prev.next = node
        ring.prev      = node
      }
      inc( node )
    }
    finally unlock()
    node
  }

  def ins( data:D ) : Hode[D]= ins( create(data) )

  private def ins( node:Hode[D] ) : Hode[D] =
  {
    if( !in(node) )
      return term

    if( isEmpty )
      add(node)
    else                   // Insert node to the tail
    {
      lock()
      try
      {
        node.prev      = ring
        node.next      = ring.next
        ring.next.prev = node
        ring.next      = node
        inc( node )
      }
      finally unlock()
    }
    node
 }

  def add( pred:Hode[D], data:D ) : Hode[D]= add( pred, create(data) )

  private def add( pred:Hode[D], node:Hode[D] ) : Hode[D]=
  {
    if( !in(pred) || !in(node) )
      return term

    lock()
    try
    {
      node.prev      = pred      // Set the node adjacent poInters
      node.next      = pred.next
      pred.next.prev = node      // Reset the poInters around the list node
      pred.next      = node
      inc( node )
    }
    finally unlock()
    node
  }

  def ins( succ:Hode[D], data:D ) : Hode[D]= ins( succ, create(data) )

  private def ins( succ:Hode[D], node:Hode[D] ) : Hode[D]=
  {
    if( !in(succ) || !in(node) )
      return term

    lock()
    try
    {
      node.prev      = succ.prev  // Set the node adjacent poInters
      node.next      = succ
      succ.prev.next = node       // Reset the poInters around the list node
      succ.prev      = node
      inc( node )
    }
    finally unlock()
    node
  }

  def del( data:D ) : Hode[D]= del( find(data)   )

  def del( node:Hode[D] ) : Hode[D]=
  {
    if( !in(node) )
      return term

    lock()
    try
    {
      node.prev.next = node.next
      node.next.prev = node.prev
      dec( node )

      node.next = term
      node.prev = term
    }
    finally unlock()
    node
  }

// ... find ...
  def find( data:D ) : Hode[D] =
  {
    var node = head
    while( in(node) )
    {
      if( node.data.equals(data) )
        return node
      node = node.next
    }
    term
  }

  def compare( cmp:(Hode[D])=>Boolean ) : Hode[D]=
  {
    var node = head
    while( in(node) )
    {
      if( cmp(node) )
        return node
      node = node.next
    }
    term
  }

  def node( idx:Int ) : Hode[D]=
  {
    if( in(idx) )
    {
      var i = 0
      var node = head
      while( in(node) )
      {
        if( i==idx )
          return node
        i += 1
        node = node.next
      }
    }
    Log.trace( 6, "Index", idx, "out of range", siz )
    term
  }

  def index( _node:Hode[D] ) : Int =
  {
    var i = 0
    var node = head
    while( in(node) )
    {
      if( _node==node )
        return i
      i += 1
      node = node.next
    }
    Log.trace( 6, "index not found for node", _node.toString )
    -1
  }

  def has( data:D ) : Boolean = in(find(data))

// ... stack ...

   def pushHead( node:Hode[D] ) { ins(node) }
   def pushHead( data:D )       { ins(data) }
   def pushTail( node:Hode[D] ) { add(node) }
   def pushTail( data:D )       { add(data) }

// ... for comprehensions ...

// forNode calls func on each node
   def forNode( func:(Hode[D]) => Unit )
   {
     var node = head
     while( in(node) )
       { func(node); node = node.next }
   }

// foreach calls func on each node data
   def foreach( func:(D) => Unit )
   {
     var node = head
     while( in(node) )
       { func(node.data); node = node.next }
   }

// Create new List by calling pred p on each List element
// and then if pred is true places the result in a new list
   def filter( pred:(D) => Boolean ): Hold[D] =
   {
     var node = head
     val hold = new Hold[D]()
     while( in(node) )
     {
       if( pred(node.data) )
         hold.add( node.copy() )
       node = node.next
     }
     hold
   }

// Create new List by calling func on each List element
// and then place the result in a new List
   def map[B]( func:(D) => B )  : Hold[B] =
   {
     var node : Hode[D]= head
     val sold : Hold[B] = new Hold[B]()
     while( in(node) )
     {
       sold.add( func(node.data) )
       node = node.next
     }
     sold
   }

// flatmap flattens List of Lists by creating a new List and then
// calls func the map the element.
// This method hard to Interpret so have to consider its implementation
/*
  def flatMap[B]( func:(D) => Iterable[B] )  : Hold[B] =
  {
     val sold : Hold[B] = new Hold[B]()
     val flat = flatMap[B]( data => func(data) )
     while( flat.hasNext )
       sold.add( flat.next )
     sold
  }
*/
 def toList : List[D] =
  {
    val list = List[D]()
    var node = tail
    while( in(node) )
    {
      node.data :: list
      node = node.prev
    }
    list
  }
  /*
  def toArray() : Array[D] =
  {
    val array = new Array[D](size)
    var i = 0
    for( data <- this )
      { array(i) = data; i+=1 }
    array
  }
  */
  def fwd( beg:Hode[D], f: Hode[D]=> Unit )
  {
    var node = beg
    while( in(node) )
      { f(node); node = node.next }
  }

  def bak( beg:Hode[D], f: Hode[D]=> Unit )
  {
    var node = beg
    while( in(node) )
      { f(node); node = node.prev }
  }

  def log()
  {
    for( data <- this )
       Log.log( data.toString )
  }

  override def toString : String =
  {
    var str : String = new String
    var node = head
    while( in(node) )
    {
       if( in(node.next) )
         str += node.toString + Text.delim
       else
         str += node.toString
       node = node.next
    }
    str
  }

  def text( tx:Text, b:String, m:String, e:String ) : Text =
  {
    tx.clear()
    tx.app(b)
    var node = head
    while( in(node) )
    {
      tx.app( node.toString )
      if( in(node.next) )
         tx.app( m )
      node = node.next
    }
    tx.app(e)
    tx
  }

  def text( tx:Text, m:String ) : Text = text( Text(100), "", m,          "" )
  def text( tx:Text           ) : Text = text( tx,        "", Text.delim, "" )
  def text(          m:String ) : Text = text( Text(100), "", m,          "" )
  def text                      : Text = text( Text(100), "", Text.delim, "" )
  def show( tx:Text           ) : Text = { tx.clear(); text(tx) }

}

// An good deque or stack based on the Java ArrayDeque

class Deque[T]
{
   private val dq:java.util.ArrayDeque[T] = new java.util.ArrayDeque[T](16)

// ... Stack ...
   def push( e:T )        { dq.push(e) }
   def pop         : T    = dq.pop
   def peek        : T    = dq.peek

// ... Deque ...
   def pushHead( e:T )         { dq.push(e) }
   def popHead          : T    = dq.pop
   def peekHead         : T    = dq.peekFirst

   def pushTail( e:T )         { dq.add(e) }
   def popTail          : T    = dq.removeLast()
   def peekTail         : T    = dq.peekLast

   def isEmpty : Boolean       = dq.isEmpty
   def clear()                 { dq.clear() }
}