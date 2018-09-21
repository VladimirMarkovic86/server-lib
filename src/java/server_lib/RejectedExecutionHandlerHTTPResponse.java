package server_lib;

import java.util.concurrent.RejectedExecutionHandler;
import java.util.concurrent.ThreadPoolExecutor;

public class RejectedExecutionHandlerHTTPResponse implements RejectedExecutionHandler {

  @Override
  public void rejectedExecution(Runnable worker, ThreadPoolExecutor executor) {
    ((clojure.lang.IFn) worker).invoke(true);
  }

}

