---
inject: true
to: <%= filePath %>
before: INJECT MSG ABOVE
sh: elm-format --yes <%= filePath %>
---
        <%= msgDef %> ->
            ( model, Cmd.none )



