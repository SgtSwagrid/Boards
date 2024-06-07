package boards.algebra

import util.structures.{StatefulFunction, UniqueId}
import util.extensions.CollectionOps.*

sealed trait Signal[+X] extends UniqueId derives CanEqual:
  
  import Signal.*
  
  final def query: Contextual[X] = for
    () <- activate
    value <- getValue
  yield value.get
  
  final def zip[Y](that: Signal[Y]): Signal[(X, Y)] = JoinedSignal(this, that)
  final def map[Y](f: X => Y): Signal[Y] = TransformedSignal(this, f)
  final def flatMap[Y](f: X => Signal[Y]): Signal[Y] = map(f).flatten
  
  protected def init: Contextual[Unit]
  protected def update(trigger: Trigger): Contextual[Unit]
  
  protected final def activate: Contextual[Unit] = for
    active <- isActive
    () <- if !active then
      for
        () <- init
        () <- update(Reactivation)
      yield ()
      else StatefulFunction.identity
  yield ()
  
  protected final def propagate(trigger: Signal[Any]): Contextual[Unit] =
    for
      val_old <- getValue
      () <- update(Propagation(trigger))
      val_new <- getValue
      subscribers <- getSubscribers
      () <- if val_old != val_new
        then subscribers.chain(_.propagate(this))
        else StatefulFunction.identity
    yield ()
  
  protected final def getContext: Contextual[SignalContext] = StatefulFunction.read
  protected final def updateContext(f: SignalContext => SignalContext): Contextual[Unit]
    = StatefulFunction.transform(f)
  
  protected final def getValue: Contextual[Option[X]] =
    getContext.map(_.getValue(this))
  
  protected final def setValue(value: Any): Contextual[Unit] =
    updateContext(_.setValue(this, value))
  
  protected final def subscribe(target: Signal[Any]*): Contextual[Unit] =
    for
      () <- target.chain(_.activate)
      () <- updateContext(_.subscribe(this, target*))
    yield ()
  
  protected final def unsubscribe(target: Signal[Any]*): Contextual[Unit] =
    updateContext(_.unsubscribe(this, target*))
    
  protected final def getSubscribers: Contextual[Set[Signal[Any]]] =
    getContext.map(_.getSubscribers(this))
  
  protected final def getSubscriptions: Contextual[Set[Signal[Any]]] =
    getContext.map(_.getSubscriptions(this))
  
  protected final def isActive: Contextual[Boolean] =
    getSubscribers.map(_.nonEmpty)

object Signal:
  
  def apply[X](initial: X): SourceSignal[X] = SourceSignal(initial)
  
  class SourceSignal[X] (
    private val initial: X
  ) extends Signal[X]:
    
    def modify(x: X): Contextual[Unit] = for
      () <- setValue(x)
      subscribers <- getSubscribers
      () <- subscribers.chain(_.propagate(this))
    yield ()
    
    protected def init: Contextual[Unit] = for
      x <- getValue
      () <- setValue(x.getOrElse(initial))
    yield ()
    
    protected def update(trigger: Trigger): Contextual[Unit] =
      StatefulFunction.identity
      
  private class JoinedSignal[X, Y] (
    left: Signal[X],
    right: Signal[Y]
  ) extends Signal[(X, Y)]:
    
    def init: Contextual[Unit] = for
      () <- subscribe(left)
      () <- subscribe(right)
    yield ()
    
    def update(trigger: Trigger): Contextual[Unit] = for
      x <- left.getValue
      y <- right.getValue
      () <- setValue((x.get, y.get))
    yield ()
  
  private class TransformedSignal[X, Y] (
    base: Signal[X],
    f: X => Y
  ) extends Signal[Y]:
    
    def init: Contextual[Unit] = for
      () <- subscribe(base)
    yield ()
    
    def update(trigger: Trigger): Contextual[Unit] = for
      x <- base.getValue
      () <- setValue(f(x.get))
    yield ()
  
  private class SwitchedSignal[X] (
    base: Signal[Signal[X]]
  ) extends Signal[X]:
    
    def init: Contextual[Unit] = for
      () <- subscribe(base)
    yield ()
    
    def update(trigger: Trigger): Contextual[Unit] =
      for
        signal <- base.getValue
        () <- trigger match
          case Propagation(`base`) | Reactivation =>
            for
              subscriptions <- getSubscriptions
              () <- unsubscribe((subscriptions - base).toSeq*)
              () <- subscribe(signal.get)
            yield ()
          case _ => StatefulFunction.identity
        x <- signal.get.getValue
        () <- setValue(x.get)
      yield ()
    
  extension [X] (signal: Signal[Signal[X]])
    def flatten: Signal[X] = SwitchedSignal[X](signal)
    
  sealed trait Trigger
  case class Propagation(from: Signal[Any]) extends Trigger
  case object Reactivation extends Trigger
  
  type Contextual[+X] = StatefulFunction[SignalContext, X]
  
  case class SignalContext (
    values: Map[Signal[Any], Any] = Map(),
    subscribers: Map[Signal[Any], Set[Signal[Any]]] = Map(),
    subscriptions: Map[Signal[Any], Set[Signal[Any]]] = Map()
  ):
    
    def getValue[X](signal: Signal[X]): Option[X] =
      values.get(signal).map(_.asInstanceOf[X])
      
    def setValue(signal: Signal[Any], value: Any): SignalContext =
      copy(values = values + (signal -> value))
    
    def getSubscribers(target: Signal[Any]): Set[Signal[Any]] =
      subscribers.getOrElse(target, Set())
    
    def getSubscriptions(subscriber: Signal[Any]): Set[Signal[Any]] =
      subscriptions.getOrElse(subscriber, Set())
    
    def subscribe(subscriber: Signal[Any], target: Signal[Any]*): SignalContext =
      target.foldLeft(this): (ctx, target) =>
        ctx.copy (
          subscribers = subscribers + (target -> (ctx.getSubscribers(target) + subscriber)),
          subscriptions = subscriptions + (subscriber -> (ctx.getSubscriptions(subscriber) + target))
        )
    
    def unsubscribe(subscriber: Signal[Any], target: Signal[Any]*): SignalContext =
      target.foldLeft(this): (ctx, target) =>
        val updated = copy (
          subscribers = subscribers + (target -> (ctx.getSubscribers(target) - subscriber)),
          subscriptions = subscriptions + (subscriber -> (ctx.getSubscriptions(subscriber) - target))
        )
        if updated.subscriptions(target).nonEmpty then updated
        else updated.unsubscribe(target, updated.getSubscriptions(target).toSeq*)