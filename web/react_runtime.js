import React from "react";
import { createRoot } from "react-dom/client";

function modifierStyle(modifiers) {
  const style = {};

  for (const modifier of modifiers || []) {
    if (modifier.type === "padding") {
      style.paddingTop = modifier.top;
      style.paddingInlineStart = modifier.start;
      style.paddingBottom = modifier.bottom;
      style.paddingInlineEnd = modifier.end;
    } else if (modifier.type === "frame") {
      if (modifier.width !== null) {
        style.width = modifier.width;
      }
      if (modifier.height !== null) {
        style.height = modifier.height;
      }
    }
  }

  return style;
}

function renderNode(node, callbacks) {
  const style = modifierStyle(node.modifiers);

  switch (node.type) {
    case "text":
      return React.createElement("p", { className: "text-node", style }, node.text);
    case "button":
      return React.createElement(
        "button",
        {
          className: "button-node",
          disabled: !node.enabled,
          onClick: () => callbacks.click(node.eventId),
          style,
        },
        node.text,
      );
    case "textField":
      return React.createElement("input", {
        className: "input-node",
        onChange: (event) => callbacks.change(node.eventId, event.target.value),
        placeholder: node.placeholder || "",
        style,
        value: node.text,
      });
    case "vstack":
    case "hstack":
      return React.createElement(
        "div",
        {
          className: node.type === "hstack" ? "stack stack-row" : "stack",
          style: { ...style, gap: node.spacing ?? 0 },
        },
        node.children.map((child, index) =>
          React.createElement(React.Fragment, { key: index }, renderNode(child, callbacks)),
        ),
      );
    case "list":
      return React.createElement(
        "div",
        { className: "todo-list", style },
        node.rows.map((row) =>
          React.createElement("div", { className: "todo-row", key: row.key }, renderNode(row.node, callbacks)),
        ),
      );
    case "scrollView":
      return React.createElement("div", { className: "scroll-view", style }, renderNode(node.child, callbacks));
    default:
      return React.createElement("pre", { className: "unknown-node", style }, JSON.stringify(node, null, 2));
  }
}

function TodosApp({ renderJson, dispatchClick, dispatchChange, rerender }) {
  const tree = JSON.parse(renderJson());
  const callbacks = {
    click(eventId) {
      dispatchClick(eventId);
      rerender();
    },
    change(eventId, value) {
      dispatchChange(eventId, value);
      rerender();
    },
  };

  return React.createElement("main", { className: "app-shell" }, renderNode(tree, callbacks));
}

export function createRenderer(rootId, renderJson, dispatchClick, dispatchChange) {
  const root = createRoot(document.getElementById(rootId));
  const renderer = {
    render() {
      root.render(
        React.createElement(TodosApp, {
          dispatchChange,
          dispatchClick,
          renderJson,
          rerender: () => renderer.render(),
        }),
      );
    },
  };

  return renderer;
}
