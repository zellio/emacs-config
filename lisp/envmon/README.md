# envmon

A GNU Emacs library to synchronize environment variables inside of Emacs with
a POSIX shell.

## Testing

From this directory:

```bash
# NB. Process tests are unimplemented at this time
command emacs --batch -L `pwd` -l envmon-test --eval "(ert-run-tests-batch-and-exit '(not (tag process)))"
```
