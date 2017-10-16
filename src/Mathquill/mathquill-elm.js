/* global MathQuill */
/* eslint-env browser */
let MQ = MathQuill.getInterface(2)

function getConfigFromAttributes (node) {
  const configMapping = {
    stringOptions: {
      leftRightIntoCmdGoes: 'left-right-into-cmd-goes',
      charsThatBreakOutOfSupSub: 'chars-that-break-out-of-sup-sub',
      autoCommands: 'auto-commands',
      autoOperatorNames: 'auto-operator-names'
    },
    boolOptions: {
      spaceBehavesLikeTab: 'space-behaves-like-tab',
      restrictMismatchedBrackets: 'restrict-mismatched-brackets',
      sumStartsWithNEquals: 'sum-starts-with-n-equals',
      supSubsRequireOperand: 'sup-subs-require-operand',
      autoSubscriptNumerals: 'auto-subscript-numerals'
    }
  }
  let config = {}
  for (let propName in configMapping.stringOptions) {
    let attrName = configMapping.stringOptions[propName]
    if (node.hasAttribute(attrName)) {
      config[propName] = node.getAttribute(attrName)
    } else {
      //config[propName] = undefined
    }
  }
  for (let propName in configMapping.boolOptions) {
    let attrName = configMapping.boolOptions[propName]
    config[propName] = node.hasAttribute(attrName)
  }
  console.log(config)
  return config
}

function initializeMathFields () {
  let nodes = document.querySelectorAll('.elm-mq-edit:not(.mq-editable-field)')
  for (let node of nodes) {
    let field = MQ.MathField(node, {
      handlers: {
        edit: function (field) {
          let event = new Event('edit')
          event.value = field.latex()
          node.dispatchEvent(event)
        },
        enter: function (field) {
          let event = new Event('enter')
          node.dispatchEvent(event)
        },
        moveOutOf: function (direction, field) {
          let event = new Event('moveOutOf')
          event.direction = direction
          node.dispatchEvent(event)
        },
        deleteOutOf: function (direction, field) {
          let event = new Event('deleteOutOf')
          event.direction = direction
          node.dispatchEvent(event)
        },
        selectOutOf: function (direction, field) {
          let event = new Event('selectOutOf')
          event.direction = direction
          node.dispatchEvent(event)
        },
        upOutOf: function (field) {
          let event = new Event('upOutOf')
          node.dispatchEvent(event)
        },
        downOutOf: function (field) {
          let event = new Event('downOutOf')
          node.dispatchEvent(event)
        }
      }
    })
    field.config(getConfigFromAttributes(node))

    let observer = new MutationObserver(function (mutations) {
      field.config(getConfigFromAttributes(node))
    })
    observer.observe(node, {attributes: true})
  }
}

function initializeStaticMaths () {
  let nodes = document.querySelectorAll('.elm-mq-static:not(.mq-math-mode)')
  for (let node of nodes) {
    let field = MQ.StaticMath(node)
    if (node.hasAttribute('content')) {
      field.latex(node.getAttribute('content'))
    }
    let observer = new MutationObserver(function (mutations) {
      if (node.hasAttribute('content')) {
        field.latex(node.getAttribute('content'))
      }
    })
    observer.observe(node, {attributes: true})
  }
}

function initializeMathquills () {
  initializeMathFields()
  initializeStaticMaths()
  window.requestAnimationFrame(initializeMathquills)
}
window.requestAnimationFrame(initializeMathquills)
