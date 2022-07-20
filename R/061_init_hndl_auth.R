init_hndl_auth(ses) {
  observeEvent(eventExpr=1, once=TRUE, handlerExpr={
    act_set_auth_state_if_returning_from_github_login(ses)

  })
}
