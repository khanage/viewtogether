module Handler.Group where

import Import

postGroupR
  :: Handler Html
postGroupR =
  undefined

getGroupR
  :: Handler Html
getGroupR =
  undefined

getExistingGroupR
  :: GroupDbId
  -> Handler Html
getExistingGroupR gid = do
  undefined

putAssignUserGroupR
  :: GroupDbId
  -> UserDbId
  -> Handler Html
putAssignUserGroupR =
  undefined

deleteAssignUserGroupR
  :: GroupDbId
  -> UserDbId
  -> Handler Html
deleteAssignUserGroupR =
  undefined
