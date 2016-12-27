module Handler.Comment where

import Import

getCommentR
  :: SessionDbId
  -> Handler Html
getCommentR sessionId =
  undefined

postCommentR
  :: SessionDbId
  -> Handler Html
postCommentR sessionId =
  undefined

getExistingCommentR
  :: SessionDbId
  -> CommentDbId
  -> Handler Html
getExistingCommentR sessionDbId commentDbId =
  error "Not yet implemented: getCommentR"

putExistingCommentR
  :: SessionDbId
  -> CommentDbId
  -> Handler Html
putExistingCommentR sessionDbId commentDbId =
  undefined

deleteExistingCommentR
  :: SessionDbId
  -> CommentDbId
  -> Handler Html
deleteExistingCommentR sessionDbId commentDbId =
  undefined

