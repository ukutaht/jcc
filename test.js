function handleErrors(err) {
  if (!err.status && !err.response) {
    notifications.error('Could not reach the server. Please check your connection')
  } else if (err.status === UNAUTHORIZED) {
    users.logout()
    browserHistory.push('/login')
  } else if (err.status === NOT_FOUND) {
    browserHistory.replace('/404')
  } else if (err.status === SERVER_ERROR) {
    notifications.error('Internal server error')
  }

  return new Promise((resolve, reject) => reject(err))
}
