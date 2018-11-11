import('./src/Main.elm')
  .then(({ Elm }) => {
    var node = document.getElementById('elm-root')
    Elm.Main.init({ node: node })
  });

(() => {
  console.log('Hi')
})()
