customHTML_head <- function() {

  HTML(
    '<style>
      .nist-print-header {
        background-color: black;
      }
      
      .nist-print-header__logo{
        margin-left:50px;
      }
      
      .title {
        font-family: "Times New Roman", Times, serif;
        text-align:center;
        font-size:50px;
      }
      </style>
      
      <div class="nist-print-header" >
      <br>
      <img class="nist-print-header__logo" width="289" height="38" margin="10" src="../nist_logo_sidestack_rev.svg" alt="National Institute of Standards and Technology" />
      <br>
      <br>
      </div>
      
      <div class="title">
      Cell Counting Analysis Tool
      <hr>
      </div>')
}

customHTML_foot <- function() {
  
  HTML(
    '
    <style>
    .nist-footer {
      background-color:black;
      color:white;
      padding-bottom:30px;
     
    }
    
    .grid-row{
      margin-left:50px;
      
    }
    
    .grid-container{
      background-color:black;
      color:white;
    }
    
    .nist-footer__logo-img{
      width: 300px;
      margin-top:30px;
    }
    
    .nist-footer__contact {
      color:white;
    }
    
    .nist-footer a{
      color:white;
      text-decoration:underline;
    }
    
    .grid-container{
      display: grid;
    }
    
    .fa {
      padding: 20px;
      font-size: 30px;
      width: 50px;
      text-align: center;
      text-decoration: none;
      margin: 5px 2px;
    }
    
    .fa:hover {
        opacity: 0.7;
    }
    
    .fa-facebook {
      background: #3B5998;
      color: white;
    }
    
    .fa-twitter {
      background: #55ACEE;
      color: white;
    }
    
    .fa-google {
      background: #dd4b39;
      color: white;
    }
    
    .fa-linkedin {
      background: #007bb5;
      color: white;
    }
    
    .fa-youtube {
      background: #bb0000;
      color: white;
    }
    
    .fa-instagram {
      background: #125688;
      color: white;
    }
    
    .fa-rss {
      background: #ff6600;
      color: white;
    }

    
    </style>
    
    <div class="nist-footer">
  
    <footer class="grid-container-nist-footer__info">
      <div class="grid-row">
        <div class="tablet:grid-col-6">
        
          <div class="nist-footer__logo">
            <a href="/" title="National Institute of Standards and Technology" rel="home">
              <img class="nist-footer__logo-img" src="../nist_logo_sidestack_rev.svg" alt="National Institute of Standards and Technology logo">
            </a>
          </div>
          
          <div class="nist-footer__contact">
            <h3 class="nist-footer__contact-heading">HEADQUARTERS</h3>
            <address>
              100 Bureau Drive<br>
              Gaithersburg, MD 20899<br>
              <a href="tel:301-975-2000">301-975-2000</a>
            </address>
            <p>
              <a href="mailto:do-webmaster@nist.gov">Webmaster</a> | 
              <a href="https://www.nist.gov/about-nist/contact-us">Contact Us</a> | 
              <a href="https://www.nist.gov/about-nist/our-organization">Our Other Offices</a> |
              <a href=" https://service.govdelivery.com/accounts/USNIST/subscriber/new">Mailing List</a>
            </p>
          </div>
        </div>
        
        <div class="tablet:grid-col-6">
          <div class="nist-footer__social-links">
          
          <link rel="stylesheet" href="https://cdnjs.cloudflare.com/ajax/libs/font-awesome/4.7.0/css/font-awesome.min.css">
            
          <a class="fa fa-twitter" href=" https://twitter.com/NIST">
          </a> 
            
          <a class="fa fa-facebook" href=" https://www.facebook.com/NIST">
          </a> 
            
          <a class="fa fa-linkedin" href=" https://www.linkedin.com/company/nist">
          </a> 
            
          <a class="fa fa-instagram" href=" https://www.instagram.com/nist/">
          </a> 
            
          <a class="fa fa-youtube" href=" https://www.youtube.com/NIST">
          </a> 
            
          <a class="fa fa-rss" href=" https://www.nist.gov/news-events/nist-rss-feeds">
          </a> 
            
          
        </div>
        
   
      </div>
    </div>
  </div>

  
    
</footer>
    '
  )
}