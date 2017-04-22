package hello;

public class Failable<T> {
  private T t;
  private String error;
  private int errorCode;

  public Failable(T t) {
    this.t = t;
  }

  public Failable(String error, int errorCode) {
    this.error = error;
    this.errorCode = errorCode;
  }

  public T getObject() {
    return this.t;
  }

  public String getError() {
    return this.error;
  }

  public int getErrorCode() {
    return this.errorCode;
  }

  public boolean succeeded() {
    return this.error == null;
  }
}
