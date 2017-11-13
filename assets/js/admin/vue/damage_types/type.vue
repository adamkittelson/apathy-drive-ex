<template>
  <tr>
    <td>
      <input v-model="type.name" ref="input" :id="type.id" :name="type.name" @input="updateName($event.target.id, $event.target.value)"></input>
    </td>
  </tr>
</template>

<script>
  export default {
    props: ['type'],
    methods: {
      updateName: _.debounce(function(id, name) {
        chan.push("update_name", {id: id, name: name})
        .receive("ok", () => console.log("Name Updated") )
      }, 500)
    }
  }
</script>