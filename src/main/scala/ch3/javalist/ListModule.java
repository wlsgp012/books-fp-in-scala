package ch3.javalist;

import java.util.function.BiFunction;
import java.util.function.Consumer;
import java.util.function.Function;
import java.util.function.Predicate;

public class ListModule {

  public static interface List<T> {

    T head();

    List<T> tail();

    boolean isEmpty();

    List<T> filter(Predicate<T> f);

    <T2> List<T2> map(Function<T, T2> f);

    <T2> T2 foldLeft(T2 seed, BiFunction<T2, T, T2> f);

    <T2> T2 foldRight(T2 seed, BiFunction<T, T2, T2> f);

    List<T> reverse();

    void foreach(Consumer<T> f);
  }

  public static final class NonEmptyList<T> implements List<T> {

    private final T _head;
    private final List<T> _tail;

    protected NonEmptyList(T head, List<T> tail) {
      this._head = head;
      this._tail = tail;
    }

    @Override
    public T head() {
      return _head;
    }

    @Override
    public List<T> tail() {
      return _tail;
    }

    @Override
    public boolean isEmpty() {
      return false;
    }

    @Override
    public List<T> filter(Predicate<T> f) {
      if (f.test(head())) {
        return list(head(), tail().filter(f));
      } else {
        return tail().filter(f);
      }
    }

    @Override
    public <T2> List<T2> map(Function<T, T2> f) {
      return list(f.apply(head()), tail().map(f));
    }

    @Override
    public <T2> T2 foldLeft(T2 seed, BiFunction<T2, T, T2> f) {
      return tail().foldLeft(f.apply(seed, head()), f);
    }

    @Override
    public <T2> T2 foldRight(T2 seed, BiFunction<T, T2, T2> f) {
      return f.apply(head(), tail().foldRight(seed, f));
    }

    @Override
    public List<T> reverse() {
      return foldLeft(emptyList(), (acc, head) -> list(head, acc));
    }

    @Override
    public void foreach(Consumer<T> f) {
      f.accept(head());
      tail().foreach(f);
    }

    @Override
    public boolean equals(Object other) {
      if (other == null || getClass() != other.getClass()) {
        return false;
      }
      List<?> that = (List<?>) other;
      return head().equals(that.head()) && tail().equals(that.tail());
    }

    @Override
    public int hashCode() {
      return 37 * (head().hashCode() + tail().hashCode());
    }

    @Override
    public String toString() {
      return "(" + head() + ", " + tail() + ")";
    }
  }

  public static class EmptyListHasNoHead extends RuntimeException {

  }

  public static class EmptyListHasNoTail extends RuntimeException {

  }

  public static final List<? extends Object> EMPTY = new List<Object>() {

    @Override
    public Object head() {
      throw new EmptyListHasNoHead();
    }

    @Override
    public List<Object> tail() {
      throw new EmptyListHasNoTail();
    }

    @Override
    public boolean isEmpty() {
      return true;
    }

    @Override
    public List<Object> filter(Predicate<Object> f) {
      return this;
    }

    @Override
    public <T2> List<T2> map(Function<Object, T2> f) {
      return emptyList();
    }

    @Override
    public <T2> T2 foldLeft(T2 seed, BiFunction<T2, Object, T2> f) {
      return seed;
    }

    @Override
    public <T2> T2 foldRight(T2 seed, BiFunction<Object, T2, T2> f) {
      return seed;
    }

    @Override
    public List<Object> reverse() {
      return emptyList();
    }

    @Override
    public void foreach(Consumer<Object> f) {
    }

    @Override
    public String toString() {
      return "()";
    }
  };

  @SuppressWarnings(value = "unchecked")
  public static <T> List<T> emptyList() {
    return (List<T>) EMPTY;
  }

  public static <T> List<T> list(T head, List<T> tail) {
    return new NonEmptyList<>(head, tail);
  }
}
