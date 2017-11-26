import {Socket} from "phoenix"

var socket = new Socket("" + (window.location.origin.replace('http', 'ws')) + "/ws");
socket.connect();

window.store = new Vuex.Store({
  state: {
    channel: socket.channel("admin:abilities", {character: characterID}),
    abilities: {},
    sortBy: "id",
    descending: false,
    page: 1
  },
  mutations: {
    updateName(state, payload) {
      state.abilities[payload.id].name = payload.name
      state.channel.push("update_name", {id: payload.id, name: payload.name})
      .receive("ok", () => console.log("Name Updated") )
    },
    updatePagination(state, payload) {
      state.sortBy = payload.sortBy
      state.descending = payload.descending
      state.page = payload.page
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
      this.$store.commit('updatePagination', {page: page, sortBy: this.$store.state.sortBy, descending: this.$store.state.descending})
      this.fetchPage()
    },
    updatePagination: function(pagination) {
      this.$store.commit('updatePagination', {page: this.$store.state.page, sortBy: pagination.sortBy, descending: !!pagination.descending})
      this.fetchPage()
    },
    fetchPage: function() {
      if (this.$store.state.channel.state === "joined") {
        console.log()
        abilities.loading = true
        var args = {query: abilities.search, page_number: this.$store.state.page, order_by: this.$store.state.sortBy, descending: this.$store.state.descending}
        this.$store.state.channel.push("fetch_page", args)
      }
    }
  },
  data() {
    return {
      pagination: {
        sortBy: 'id'
      },
      search: '',
      loading: true,
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
  abilities.loading = false
  store.state.abilities = page.entries;
  abilities.pagination = {
    page: page.page_number,
    rowsPerPage: page.page_size,
    totalItems: page.total_entries,
    totalPages: page.total_pages
  }
});
