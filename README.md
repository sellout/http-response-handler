I find myself writing bits and pieces of this each time I write something that makes HTTP requests. I end up scattering `COND`s and `CASE`s around to handle all the possible errors I might get back from the server appropriately. So this tries to make that a bit nicer.

It is designed around the result of [Drakma](http://weitz.de/drakma/)’s [`HTTP-REQUEST`](http://weitz.de/drakma/#http-request) function, because it is the most popular HTTP client library, but it can easily be used with any library out there.

The entry point is `HANDLE-HTTP-RESPONSE`.

Say you have some foolishly-optimistic code, like:

```common-lisp
(parse-json (drakma:http-request ...))
```

With this library, just insert `HANDLE-HTTP-REQUEST`

```common-lisp
(parse-json (multiple-value-call #'handle-http-response
                                 (drakma:http-request ...)))
```

And now, when something goes wrong, you’ll see

```
Received a "Method Not Allowed" (HTTP 405) error from `http://example.com`:
the only methods allowed for this resource are GET, HEAD.
   [Condition of type HTTP-METHOD-NOT-ALLOWED]
```

which is already a step up from getting a JSON error about not being able to parse whatever the server sent back. And with just a bit more work:

```common-lisp
(parse-json (handler-case (multiple-value-call #'handle-http-response
                                               (drakma:http-request ...))
              (retryable-http-error (e)
                (let ((delay (seconds-until-retry e))
                      (restart (find-restart 'retry-after)))
                  (if (and delay restart)
                      (invoke-restart restart delay)
                      (error e))))))
```

