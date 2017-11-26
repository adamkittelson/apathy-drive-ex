import {Socket} from "phoenix"

var socket = new Socket("" + (window.location.origin.replace('http', 'ws')) + "/ws");
socket.connect();

window.store = new Vuex.Store({
  state: {
    channel: socket.channel("admin:abilities", {character: characterID}),
    abilities: {}
  },
  mutations: {
    updateName(state, payload) {
      state.abilities[payload.id].name = payload.name
      state.channel.push("update_name", {id: payload.id, name: payload.name})
      .receive("ok", () => console.log("Name Updated") )
    }
  }
})

window.abilities = new Vue({
  el: "#app",
  store,
  template: '#abilities',
  computed: {
    abilities: function() {
      return Object.values(this.$store.state.abilities)
    }
  },
  methods: {
    updateName: function(id, name) {
      this.$store.commit('updateName', {id: id, name: name})
    },
    updatePageNumber: function(page) {
      this.$store.state.channel.push("fetch_page", {page_number: page})
    }
  },
  data() {
    return {
      pagination: {
        sortBy: 'id'
      },
      headers: [
        {
          text: 'ID',
          align: 'left',
          value: 'id'
        },
        { text: 'Name', align: 'left', value: 'name' }
      ]
    }
  }
});

store.state.channel.join().receive("error", ({reason}) => window.location = "" + window.location.origin )

store.state.channel.on("abilities", function(page){
  store.state.abilities = page.entries;
  abilities.pagination = {
    page: page.page_number,
    rowsPerPage: page.page_size,
    totalItems: page.total_entries,
    totalPages: page.total_pages
  }
});
