---
inject: true
to: <%= filePath %>
before: INJECT UPDATE CASE ABOVE
sh: elm-format --yes <%= filePath %>
---
        <%= msgDef %> ->
            ( model, Cmd.none )



