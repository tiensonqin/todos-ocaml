import React from "react";
import { createRoot } from "react-dom/client";

function TodoRow({ todo, onDelete, onToggle }) {
  return React.createElement(
    "li",
    { className: `todo-row${todo.completed ? " completed" : ""}` },
    React.createElement(
      "button",
      {
        "aria-label": todo.completed ? "Mark incomplete" : "Mark complete",
        className: "icon-button",
        onClick: () => onToggle(todo.id),
        type: "button",
      },
      todo.completed ? "✓" : "",
    ),
    React.createElement("span", null, todo.title),
    React.createElement(
      "button",
      {
        className: "delete-button",
        onClick: () => onDelete(todo.id),
        type: "button",
      },
      "Delete",
    ),
  );
}

function TodoColumn({ emptyText, onDelete, onToggle, title, todos }) {
  return React.createElement(
    "section",
    null,
    React.createElement("h2", null, title),
    todos.length === 0
      ? React.createElement("p", { className: "muted" }, emptyText)
      : React.createElement(
          "ul",
          null,
          todos.map((todo) =>
            React.createElement(TodoRow, {
              key: todo.id,
              onDelete,
              onToggle,
              todo,
            }),
          ),
        ),
  );
}

function TodosApp({ onAdd, onDelete, onDraftChange, onToggle, state }) {
  return React.createElement(
    "main",
    { className: "app-shell" },
    React.createElement(
      "aside",
      { className: "sidebar" },
      React.createElement("h1", null, "Todos"),
      React.createElement("p", { className: "counter" }, `${state.activeCount} active`),
      React.createElement("p", { className: "counter muted" }, `${state.completedCount} completed`),
    ),
    React.createElement(
      "section",
      { className: "workspace" },
      React.createElement(
        "form",
        {
          className: "composer",
          onSubmit: (event) => {
            event.preventDefault();
            onAdd();
          },
        },
        React.createElement("input", {
          "aria-label": "New task",
          onChange: (event) => onDraftChange(event.target.value),
          placeholder: "New task",
          value: state.draft,
        }),
        React.createElement("button", { type: "submit" }, "Add"),
      ),
      React.createElement(
        "div",
        { className: "columns" },
        React.createElement(TodoColumn, {
          emptyText: "Nothing active right now.",
          onDelete,
          onToggle,
          title: "Active",
          todos: state.activeTodos,
        }),
        React.createElement(TodoColumn, {
          emptyText: "Nothing completed yet.",
          onDelete,
          onToggle,
          title: "Done",
          todos: state.completedTodos,
        }),
      ),
    ),
  );
}

export function createTodoRenderer(rootId, getStateJson, onDraftChange, onAdd, onToggle, onDelete) {
  const root = createRoot(document.getElementById(rootId));

  return {
    render() {
      root.render(
        React.createElement(TodosApp, {
          onAdd,
          onDelete,
          onDraftChange,
          onToggle,
          state: JSON.parse(getStateJson()),
        }),
      );
    },
  };
}
